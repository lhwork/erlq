import pylibmc
import cPickle as pickle
import msgpack
import random

mc = pylibmc.Client(['127.0.0.1:11211'])

data = mc.get('qq_push_queue')

#print pickle.loads(data)
print data
print msgpack.unpackb(data)
