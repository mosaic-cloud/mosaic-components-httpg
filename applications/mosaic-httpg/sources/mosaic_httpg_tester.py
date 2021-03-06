
import json
import os
import pprint
import random
import string
import struct
import sys
import time

import pika


_verbose = True
_broker_host = "127.0.0.1"
_broker_port = 21688
_broker_user = "guest"
_broker_password = "guest"
_broker_virtual_host = "/"
_handlers_exchange_identifier = "mosaic-http-requests"
_handlers_queue_identifier = "mosaic-http-requests"
_handlers_queue_routing_key = "#"
_reconnect_sleep = 1
_consume_sleep = 1
_glitch_probability_ = 0.0


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
		
		def _handle (_channel, _method, _properties, _body) :
			
			if _verbose : print >> sys.stderr, "[  ] handling..."
			_request_data = _body
			_request_content_type = _properties.content_type
			_request_content_encoding = _properties.content_encoding
			_response_data, _response_content_type, _response_content_encoding, _callback_exchange, _callback_routing_key \
					= _handle_message (_request_data, _request_content_type, _request_content_encoding)
			if _verbose : print >> sys.stderr, "[  ] publishing: `%s` <- `%s`..." % (_callback_exchange, _callback_routing_key)
			_channel.basic_publish (
					_callback_exchange, _callback_routing_key, _response_data,
					properties = pika.BasicProperties (content_type = _response_content_type, content_encoding = _response_content_encoding),
					mandatory = False, immediate = False)
			_channel.basic_ack (delivery_tag = _method.delivery_tag, multiple = False)
			return
		
		# _channel.basic_qos (prefetch_size = 0, prefetch_count = 16, global_ = False)
		
		if False :
			
			# while _connection.is_alive () :
			while True :
				
				_outcome = None
				
				try :
					if _verbose : print >> sys.stderr, "[  ] polling..."
					_outcome = _channel.basic_get (queue = _handlers_queue_identifier)
				except Exception as _error :
					del _outcome
					if _verbose : print >> sys.stderr, "[ee] failed while polling: %r; exiting loop..." % (_error,)
					break
				
				if isinstance (_outcome, pika.spec.Basic.GetOk) :
					_handle (_channel, _outcome, _outcome.get_properties (), _outcome.get_body ())
				
				elif isinstance (_outcome, pika.spec.Basic.GetEmpty) :
					if _verbose : print >> sys.stderr, "[  ] nothing; sleeping..."
					time.sleep (_consume_sleep)
					
				else :
					print >> sys.stderr, "[ee] unexpected polling outcome: %r; ignoring" % (_outcome,)
				
				del _outcome
			
		else :
			
			_channel.basic_consume (_handle, queue = _handlers_queue_identifier, exclusive = False, no_ack = False)
			
			_channel.start_consuming ()
		
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
	
	_glitch = _maybe_glitch (_response, _callback_identifier, _response_data, _response_content_type, _response_content_encoding)
	if _glitch is not None :
		_response_data, _response_content_type, _response_content_encoding = _glitch
	
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
	
	assert _decoded_headers.get ('version') == 1
	
	_http_body_type = _decoded_headers.get ('http-body')
	
	if _http_body_type == 'empty' :
		
		assert _data_size == _encoded_headers_limit
		
		_encoded_body = ''
		_encoded_body_size = len (_encoded_body)
	
	elif _http_body_type == 'embedded' :
		
		assert _data_size == _encoded_headers_limit
		
		_encoded_body = _decoded_headers.get ('http-body-content')
		_encoded_body_size = len (_encoded_body)
	
	elif _http_body_type == 'following' :
		
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
			socket_remote_ip = _decoded_headers.get ('socket-remote-ip'),
			socket_remote_port = _decoded_headers.get ('socket-remote-port'),
			socket_remote_fqdn = _decoded_headers.get ('socket-remote-fqdn'),
			socket_local_ip = _decoded_headers.get ('socket-local-ip'),
			socket_local_port = _decoded_headers.get ('socket-local-port'),
			socket_local_fqdn = _decoded_headers.get ('socket-local-fqdn'),
			http_version = _decoded_headers.get ('http-version'),
			http_method = _decoded_headers.get ('http-method'),
			http_uri = _decoded_headers.get ('http-uri'),
			http_headers = _decoded_headers.get ('http-headers'),
			http_body = _decoded_body)
	
	_callback_identifier = str (_decoded_headers.get ('callback-identifier'))
	_callback_exchange = str (_decoded_headers.get ('callback-exchange'))
	_callback_routing_key = str (_decoded_headers.get ('callback-routing-key'))
	
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
	
	_body = "Ok: pid = %d, time = %f" % (os.getpid (), time.time ())
	
	_response = _Response (
			http_version = _request.http_version,
			http_code = 200,
			http_status = "Ok",
			http_headers = {
				"Content-Length" : str (len (_body)),
				"Content-Type" : "text/plain",
			},
			http_body = _body)
	
	return _response


def _maybe_glitch (_response_, _callback_identifier_, _response_data_, _response_content_type_, _response_content_encoding_) :
	
	global _glitch_probability_
	
	if random.random () > _glitch_probability_ :
		sys.stderr.write ('.')
		return None
	
	sys.stderr.write ('!')
	
	_response_data = None
	_response_content_type = None
	_response_content_encoding = None
	
	_response_headers_data = None
	_response_headers_size = None
	_response_body_data = None
	_response_body_size = None
	
	_response_headers = {
			"version" : 1,
			"callback-identifier" : _callback_identifier_,
			"http-version" : _response_.http_version,
			"http-code" : _response_.http_code,
			"http-status" : _response_.http_status,
			"http-headers" : _response_.http_headers,
			"http-body" : "following"}
	
	_response_body = None
	
	if not hasattr (_maybe_glitch, "_glitches") :
		_glitches = [
				('content-type/none', 0.1), ('content-type/random', 0.1), ('content-type/garbage', 0.1),
				('content-encoding/none', 0.1), ('content-encoding/random', 0.1), ('content-encoding/garbage', 0.1),
				('response-headers/version', 0.1), ('response-headers/callback-identifier', 0.1),
				('response-headers/http-version', 0.1), ('response-headers/http-code', 0.0), ('response-headers/http-status', 0.1),
				('response-headers/http-headers', 0.1), ('response-headers/http-body', 0.1), ('response-headers/http-body-content', 0.1),
				('response-body/none', 0.01), ('response-body/random', 0.01), ('response-body/garbage', 0.01),
				('response-data/none', 0.01), ('response-data/random', 0.01), ('response-data/garbage', 0.01),
				('response-headers-data/none', 0.01), ('response-headers-data/random', 0.01), ('response-headers-data/garbage', 0.01),
				('response-headers-data/size', 0.01), ('response-body-data/size', 0.01)]
		_sum = 0.0
		for _glitch_identifier, _glitch_probability in _glitches :
			_sum += _glitch_probability
		for _glitch_index in xrange (len (_glitches)) :
			_glitches[_glitch_index] = (_glitches[_glitch_index][0], _glitches[_glitch_index][1] / _sum)
		_maybe_glitch._glitches = _glitches
	else :
		_glitches = _maybe_glitch._glitches
	
	while True :
		
		_glitch = None
		_glitch_remaining_probability = 1.0
		for _glitch_identifier, _glitch_probability in _glitches :
			if random.random () <= (_glitch_probability / _glitch_remaining_probability) :
				_glitch = _glitch_identifier
				break
			_glitch_remaining_probability -= _glitch_probability
		assert _glitch is not None
		
		if _glitch == 'content-type/none' :
			if _response_content_type is not None :
				continue
			_response_content_type = ''
		elif _glitch == 'content-type/random' :
			if _response_content_type is not None :
				continue
			_response_content_type = _generate_printable_string (1, 64)
		elif _glitch == 'content-type/garbage' :
			if _response_content_type is not None :
				continue
			_response_content_type = _generate_garbage_string (1, 64)
		
		elif _glitch == 'content-encoding/none' :
			if _response_content_encoding is not None :
				continue
			_response_content_encoding = ''
		elif _glitch == 'content-encoding/random' :
			if _response_content_encoding is not None :
				continue
			_response_content_encoding = _generate_printable_string (1, 64)
		elif _glitch == 'content-encoding/garbage' :
			if _response_content_encoding is not None :
				continue
			_response_content_encoding = _generate_garbage_string (1, 64)
		
		elif _glitch == 'response-data/none' :
			if _response_data is not None :
				continue
			_response_data = ''
		elif _glitch == 'response-data/random' :
			if _response_data is not None :
				continue
			_response_data = _generate_printable_string (1, 128)
		elif _glitch == 'response-data/garbage' :
			if _response_data is not None :
				continue
			_response_data = _generate_garbage_string (1, 128)
		
		elif _glitch == 'response-headers-data/none' :
			if _response_headers_data is not None :
				continue
			_response_headers_data = ''
		elif _glitch == 'response-headers-data/random' :
			if _response_headers_data is not None :
				continue
			_response_headers_data = _generate_printable_string (1, 128)
		elif _glitch == 'response-headers-data/garbage' :
			if _response_headers_data is not None :
				continue
			_response_headers_data = _generate_garbage_string (1, 128)
		
		elif _glitch == 'response-headers-data/size' :
			if _response_headers_size is not None :
				continue
			_response_headers_size = random.randint (0, 1 << 32 - 1)
		elif _glitch == 'response-body-data/size' :
			if _response_headers_size is not None :
				continue
			_response_body_size = random.randint (0, 1 << 32 - 1)
		
		elif _glitch == 'response-headers/version' :
			_response_headers['version'] = _generate_random_json ()
		elif _glitch == 'response-headers/callback-identifier' :
			_response_headers['callback-identifier'] = _generate_random_json ()
		elif _glitch == 'response-headers/http-version' :
			_response_headers['http-version'] = _generate_random_json ()
		elif _glitch == 'response-headers/http-code' :
			_response_headers['http-code'] = _generate_random_json ()
		elif _glitch == 'response-headers/http-status' :
			_response_headers['http-status'] = _generate_random_json ()
		elif _glitch == 'response-headers/http-headers' :
			_response_headers['http-headers'] = _generate_random_json ()
		elif _glitch == 'response-headers/http-body' :
			_response_headers['http-body'] = _generate_random_json ()
		elif _glitch == 'response-headers/http-body-content' :
			_response_headers['http-body-content'] = _generate_random_json ()
		
		elif _glitch == 'response-body/none' :
			if _response_body is not None :
				continue
			_response_body = ''
		elif _glitch == 'response-body/random' :
			if _response_body is not None :
				continue
			_response_body = _generate_printable_string (1, 128)
		elif _glitch == 'response-body/garbage' :
			if _response_body is not None :
				continue
			_response_body = _generate_garbage_string (1, 128)
		
		else :
			print >> sys.stderr, '[ee] unknown glitch: ' + _glitch
		
		if _response_data is not None :
			break
		
		if random.random > 0.2 :
			break
	
	if _response_data is None :
		
		if _response_headers_data is None :
			_response_headers_data = json.dumps (_response_headers, False, True, False, True, None, None, None, 'utf-8')
			if _response_headers_size is None :
				_response_headers_size = len (_response_headers_data)
			_response_headers_data = struct.pack (">L", _response_headers_size) + _response_headers_data
		
		if _response_body_data is None :
			if _response_body is None :
				_response_body = _response_.http_body
			_response_body_data = _response_body
			if _response_body_size is None :
				_response_body_size = len (_response_body_data)
			_response_body_data = struct.pack (">L", _response_body_size) + _response_body_data
		
		_response_data = _response_headers_data + _response_body_data
	
	if _response_content_type is None :
		_response_content_type = _response_content_type_
	
	if _response_content_encoding is None :
		_response_content_encoding = _response_content_encoding_
	
	return _response_data, _response_content_type, _response_content_encoding


def _generate_printable_string (_min_length, _max_length) :
	return ''.join ([chr (random.randint (32, 127)) for i in xrange (random.randint (_min_length, _max_length))])

def _generate_garbage_string (_min_length, _max_length) :
	return ''.join ([chr (random.randint (0, 255)) for i in xrange (random.randint (_min_length, _max_length))])

def _generate_random_json (_depth_probability = 1.0) :
	if random.random () < _depth_probability :
		_choice = random.randint (0, 5)
	else :
		_choice = random.randint (0, 3)
	if _choice == 0 :
		return _generate_printable_string (1, 32)
	elif _choice == 1 :
		return random.randint (-1 << 31, 1 << 31 - 1)
	elif _choice == 2 :
		return random.random () * random.randint (-1 << 31, 1 << 31 - 1)
	elif _choice == 3 :
		return random.choice ([True, False, None])
	elif _choice == 4 :
		return [_generate_random_json (_depth_probability * 0.01) for i in xrange (0, 128)]
	elif _choice == 5 :
		_dict = {}
		for i in xrange (0, 128) :
			_dict[_generate_printable_string (1, 32)] = _generate_random_json (_depth_probability * 0.01)
		return _dict
	else :
		assert False
		return None


if __name__ == '__main__' :
	assert len (sys.argv) == 1
	_loop ()
