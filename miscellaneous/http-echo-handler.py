
import sys
import time

import pika

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

def _main () :
	
	while True :
		
		_connection = None
		_channel = None
		try :
			print >> sys.stderr, "[  ] connecting..."
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
			print >> sys.stderr, "[ee] failed while connecting: `%r`; sleeping and then reconnecting..." % (_error,)
			time.sleep (_reconnect_sleep)
			continue
		
		try :
			print >> sys.stderr, "[  ] declaring..."
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
			print >> sys.stderr, "[ee] failed while declaring: `%r`; aborting!" % (_error,)
			exit (1)
		
		while _connection.is_alive () :
			
			_outcome = None
			try :
				print >> sys.stderr, "[  ] consuming..."
				_outcome = _channel.basic_get (queue = _handlers_queue_identifier)
			except Exception as _error :
				del _outcome
				print >> sys.stderr, "[ee] failed while consuming: `%r`; exiting loop..." % (_error,)
				break
			
			if isinstance (_outcome, pika.spec.Basic.GetOk) :
				print >> sys.stderr, "[  ] handling..."
				_channel.basic_ack (delivery_tag = _outcome.delivery_tag)
				_response = _handle_message (_outcome.get_body ())
				
			elif isinstance (_outcome, pika.spec.Basic.GetEmpty) :
				print >> sys.stderr, "[  ] nothing; sleeping..."
				time.sleep (_consume_sleep)
				
			else :
				print >> sys.stderr, "[ee] unexpected outcome: `%r`; ignoring" % (_outcome,)
			
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


def _handle_message (_data) :
	print >> sys.stderr, "[  ] received: `%r`;" % (_data)
	pass


if __name__ == '__main__' :
	_main ()
