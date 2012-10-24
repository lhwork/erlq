import pylibmc
import cPickle as pickle
import msgpack
import random

mc = pylibmc.Client(['127.0.0.1:11211'])

data = {'uid':'rekoo1','name':'test','fids':[str(x) for x in xrange(5700090,5700095)],'data':{'type':3,'msg':'kdddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddkddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd'}}
#data = "test"
#mc.set('qq_push_queue',pickle.dumps(data))
val1 = pickle.dumps(data)
print len(val1)
val = msgpack.packb(data)
print len(val)
 