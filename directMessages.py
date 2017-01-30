import tweepy
import pandas as pd
import numpy as np
import json
import itertools
import urllib
import codecs
import time
from itertools import chain

consumer_key        = 'vDfPjIl7fRMjwHwYfj0rz5Vid'
consumer_secret     = 'W5ojxlgXd9xTo7oVX95Ni3nbB6UrkODMtfSe3F6FF2NcEbAgWB'
access_token        = '137735852-QmQl1gg51p8Za2W68d4akeH0MbaH7VNOSc9Y5KOK'
access_token_secret = 'gFAJhf8iuwh1JIdYUFFxTwnLSgouStPrxnnAbEs2bfRMt'

auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_token_secret)
api = tweepy.API(auth, parser= tweepy.parsers.JSONParser())
prueba = api.direct_messages()

dm = api.direct_messages(count=200)
dm_general = []
for i in dm:
    temp_res = dict(id_message = i['id'], fecha = i['created_at'])
    temp_res.update(nombre_destinatario = i['recipient']['id'])
    temp_res.update(id_destinatario = i['recipient']['id'])
    temp_res.update(nombre_mensajero = i['sender_screen_name'])
    temp_res.update(id_mensajero = i['sender_id'])
    temp_res.update(texto=i['text'])
    dm_general.append(temp_res)

dm_general = pd.DataFrame.from_dict(dm_general)
dm_general.to_csv("dm.csv", encoding="utf-8")







