
import json
import pprint
import struct
import sys
import time

import pika


_verbose = False
_broker_host = "127.0.0.1"
_broker_port = 5672
_broker_user = "guest"
_broker_password = "guest"
_broker_virtual_host = "/"
_handlers_exchange_identifier = "mosaic-http-requests"
_handlers_queue_identifier = "mosaic-http-requests"
_handlers_queue_routing_key = "#"
_reconnect_sleep = 1
_consume_sleep = 1


def _loop () :
	
	while True :
		
		_connection = None
		_channel = None
		try :
			if _verbose : print >> sys.stderr, "[  ] connecting..."
			_connection = pika.BlockingConnection (pika.ConnectionParameters (
					_broker_host, port = _broker_port, virtual_host = _broker_virtual_host,
					credentials = pika.PlainCredentials (_broker_user, _broker_password)))
			_channel = _connection.channel ()
		except Exception as _error :
			if _connection is not None :
				try :
					_connection.close ()
				except :
					pass
			del _connection
			del _channel
			if _verbose : print >> sys.stderr, "[ee] failed while connecting: %r; sleeping and then reconnecting..." % (_error,)
			time.sleep (_reconnect_sleep)
			continue
		
		try :
			if _verbose : print >> sys.stderr, "[  ] declaring..."
			_channel.exchange_declare (
					exchange = _handlers_exchange_identifier, type = "topic",
					durable = False, auto_delete = False)
			_channel.queue_declare (
					queue = _handlers_queue_identifier,
					exclusive = False, durable = False, auto_delete = False)
			_channel.queue_bind (
					queue = _handlers_queue_identifier, exchange = _handlers_exchange_identifier,
					routing_key = _handlers_queue_routing_key)
		except Exception as _error :
			print >> sys.stderr, "[ee] failed while declaring: %r; aborting!" % (_error,)
			exit (1)
		
		while _connection.is_alive () :
			_outcome = None
			try :
				if _verbose : print >> sys.stderr, "[  ] polling..."
				_outcome = _channel.basic_get (queue = _handlers_queue_identifier)
			except Exception as _error :
				del _outcome
				if _verbose : print >> sys.stderr, "[ee] failed while polling: %r; exiting loop..." % (_error,)
				break
			
			if isinstance (_outcome, pika.spec.Basic.GetOk) :
				if _verbose : print >> sys.stderr, "[  ] handling..."
				_request_data = _outcome.get_body ()
				_request_content_type = _outcome.get_properties () .content_type
				_request_content_encoding = _outcome.get_properties () .content_encoding
				_response_data, _response_content_type, _response_content_encoding, _callback_exchange, _callback_routing_key \
						= _handle_message (_request_data, _request_content_type, _request_content_encoding)
				if _verbose : print >> sys.stderr, "[  ] publishing: `%s` <- `%s`..." % (_callback_exchange, _callback_routing_key)
				_channel.basic_publish (
						_callback_exchange, _callback_routing_key, _response_data,
						properties = pika.BasicProperties (content_type = _response_content_type, content_encoding = _response_content_encoding),
						mandatory = False, immediate = False,
						block_on_flow_control = True)
				_channel.basic_ack (delivery_tag = _outcome.delivery_tag)
				
			elif isinstance (_outcome, pika.spec.Basic.GetEmpty) :
				if _verbose : print >> sys.stderr, "[  ] nothing; sleeping..."
				time.sleep (_consume_sleep)
				
			else :
				print >> sys.stderr, "[ee] unexpected polling outcome: %r; ignoring" % (_outcome,)
			
			del _outcome
		
		try :
			_channel.close ()
		except :
			pass
		try :
			_connection.close ()
		except :
			pass
		del _connection
		del _channel
	
	return


def _handle_message (_request_data, _request_content_type, _request_content_encoding) :
	
	_request, _callback_identifier, _callback_exchange, _callback_routing_key \
			= _decode_request_message_body (_request_data, _request_content_type, _request_content_encoding)
	
	_response = _process (_request)
	
	_response_data, _response_content_type, _response_content_encoding \
			= _encode_response_message_body (_response, _callback_identifier)
	
	return (_response_data, _response_content_type, _response_content_encoding, _callback_exchange, _callback_routing_key)


def _encode_response_message_body (_response, _callback_identifier) :
	
	if _verbose : print >> sys.stderr, "[  ] encoding message:"
	
	_decoded_headers = {
		"version" : 1,
		"callback-identifier" : _callback_identifier,
		"http-version" : _response.http_version,
		"http-code" : _response.http_code,
		"http-status" : _response.http_status,
		"http-headers" : _response.http_headers,
		"http-body" : "following"
	}
	
	if _verbose : print >> sys.stderr, "[  ]     -> decoded headers:"
	if _verbose : pprint.pprint (_decoded_headers, sys.stderr)
	
	_decoded_body = _response.http_body
	
	if _verbose : print >> sys.stderr, "[  ]     -> decoded body:"
	if _verbose : print >> sys.stderr, _decoded_headers
	
	_encoded_headers = json.dumps (_decoded_headers, False, True, False, True, None, None, None, 'utf-8')
	_encoded_headers_size = len (_encoded_headers)
	
	if _verbose : print >> sys.stderr, "[  ]     -> encoded headers size: %d" % (_encoded_headers_size,)
	if _verbose : print >> sys.stderr, "[  ]     -> encoded headers: %r" % (_encoded_headers,)
	
	_encoded_body = _response.http_body
	_encoded_body_size = len (_encoded_body)
	
	if _verbose : print >> sys.stderr, "[  ]     -> encoded body size: %d" % (_encoded_body_size,)
	if _verbose : print >> sys.stderr, "[  ]     -> encoded body: %r" % (_encoded_body,)
	
	_data = ''.join ([
			struct.pack (">L", _encoded_headers_size),
			_encoded_headers,
			struct.pack (">L", _encoded_body_size),
			_encoded_body])
	_data_size = len (_data)
	
	if _verbose : print >> sys.stderr, "[  ]     -> data size: %d" % (_data_size)
	if _verbose : print >> sys.stderr, "[  ]     -> data: %r" % (_data,)
	
	_content_type = 'application/octet-stream'
	_content_encoding = 'binary'
	
	if _verbose : print >> sys.stderr, "[  ]     -> content type: %r;" % (_content_type,)
	if _verbose : print >> sys.stderr, "[  ]     -> content encoding: %r;" % (_content_encoding,)
	
	return (_data, _content_type, _content_encoding)


def _decode_request_message_body (_data, _content_type, _content_encoding) :
	
	if _verbose : print >> sys.stderr, "[  ] decoding message:"
	
	if _verbose : print >> sys.stderr, "[  ]     -> content type: %r;" % (_content_type,)
	if _verbose : print >> sys.stderr, "[  ]     -> content encoding: %r;" % (_content_encoding,)
	
	_data_size = len (_data)
	
	if _verbose : print >> sys.stderr, "[  ]     -> data size: %d;" % (_data_size,)
	if _verbose : print >> sys.stderr, "[  ]     -> data: %r;" % (_data,)
	
	assert _content_type == 'application/octet-stream'
	assert _content_encoding == 'binary'
	assert _data_size >= 4
	
	_encoded_headers_size = struct.unpack (">L", _data[0:4]) [0]
	_encoded_headers_offset = 4
	_encoded_headers_limit = _encoded_headers_offset + _encoded_headers_size
	
	assert _data_size >= _encoded_headers_limit
	
	_encoded_headers = _data[_encoded_headers_offset : _encoded_headers_limit]
	
	if _verbose : print >> sys.stderr, "[  ]     -> encoded headers size: %d;" % (_encoded_headers_size,)
	if _verbose : print >> sys.stderr, "[  ]     -> encoded headers: %r;" % (_encoded_headers,)
	
	_decoded_headers = json.loads (_encoded_headers, 'utf-8')
	
	if _verbose : print >> sys.stderr, "[  ]     -> decoded headers: %r;" % (_decoded_headers,)
	if _verbose : print >> sys.stderr, "[  ]     -> decoded headers:"
	if _verbose : pprint.pprint (_decoded_headers, sys.stderr)
	
	assert _decoded_headers['version'] == 1
	
	if _decoded_headers['http-body'] == 'empty' :
		
		assert _data_size == _encoded_headers_limit
		
		_encoded_body = ''
		_encoded_body_size = len (_encoded_body)
	
	elif _decoded_headers['http-body'] == 'embedded' :
		
		assert _data_size == _encoded_headers_limit
		
		_encoded_body = _decoded_headers['http-body-content']
		_encoded_body_size = len (_encoded_body)
	
	elif _decoded_headers['http-body'] == 'following' :
		
		assert _data_size >= _encoded_headers_limit + 4
		
		_encoded_body_size = struct.unpack (">L", _data[_encoded_headers_limit : _encoded_headers_limit + 4]) [0]
		_encoded_body_offset = _encoded_headers_limit + 4
		_encoded_body_limit = _encoded_body_offset + _encoded_body_size
		
		assert _data_size == _encoded_body_limit
		
		_encoded_body = _data[_encoded_body_offset : 4 + _encoded_body_limit]
	
	else :
		assert False
	
	if _verbose : print >> sys.stderr, "[  ]     -> encoded body size: %d;" % (_encoded_body_size,)
	if _verbose : print >> sys.stderr, "[  ]     -> encoded body: %r;" % (_encoded_body,)
	
	_decoded_body = _encoded_body
	
	if _verbose : print >> sys.stderr, "[  ]     -> decoded body:"
	if _verbose : print >> sys.stderr, _decoded_body
	
	_request = _Request (
			socket_remote_ip = _decoded_headers['socket-remote-ip'],
			socket_remote_port = _decoded_headers['socket-remote-port'],
			socket_remote_fqdn = _decoded_headers['socket-remote-fqdn'],
			socket_local_ip = _decoded_headers['socket-local-ip'],
			socket_local_port = _decoded_headers['socket-local-port'],
			socket_local_fqdn = _decoded_headers['socket-local-fqdn'],
			http_version = _decoded_headers['http-version'],
			http_method = _decoded_headers['http-method'],
			http_uri = _decoded_headers['http-uri'],
			http_headers = _decoded_headers['http-headers'],
			http_body = _decoded_body)
	
	_callback_identifier = str (_decoded_headers['callback-identifier'])
	_callback_exchange = str (_decoded_headers['callback-exchange'])
	_callback_routing_key = str (_decoded_headers['callback-routing-key'])
	
	if _verbose : print >> sys.stderr, "[  ]     -> callback identifier: %r;" % (_callback_identifier,)
	if _verbose : print >> sys.stderr, "[  ]     -> callback exchange: %r;" % (_callback_exchange,)
	if _verbose : print >> sys.stderr, "[  ]     -> callback routing key: %r;" % (_callback_routing_key,)
	
	return (_request, _callback_identifier, _callback_exchange, _callback_routing_key)


class _Request (object) :
	
	def __init__ (self,
			socket_remote_ip = None, socket_remote_port = None, socket_remote_fqdn = None,
			socket_local_ip = None, socket_local_port = None, socket_local_fqdn = None,
			http_version = None, http_method = None, http_uri = None,
			http_headers = None, http_body = None) :
		self.socket_remote_ip = socket_remote_ip
		self.socket_remote_port = socket_remote_port
		self.socket_remote_fqdn = socket_remote_fqdn
		self.socket_local_ip = socket_local_ip
		self.socket_local_port = socket_local_port
		self.socket_local_fqdn = socket_local_fqdn
		self.http_version = http_version
		self.http_method = http_method
		self.http_uri = http_uri
		self.http_headers = http_headers
		self.http_body = http_body
		return


class _Response (object) :
	
	def __init__ (self,
			http_version = None, http_code = None, http_status = None,
			http_headers = None, http_body = None) :
		self.http_version = http_version
		self.http_code = http_code
		self.http_status = http_status
		self.http_headers = http_headers
		self.http_body = http_body
		return


def _process (_request) :
	
	if _verbose : print >> sys.stderr, "[  ] processing:"
	if _verbose : print >> sys.stderr, "[  ]     -> method: %s" % (_request.http_method,)
	if _verbose : print >> sys.stderr, "[  ]     -> uri: %s" % (_request.http_uri,)
	
	_response = _Response (
			http_version = _request.http_version,
			http_code = 200,
			http_status = "Ok",
			http_headers = {},
			http_body = "Ok")
	
	return _response


if __name__ == '__main__' :
	assert len (sys.argv) == 1
	_loop ()
