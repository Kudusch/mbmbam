#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import feedparser
import re
import csv
from datetime import datetime

def prase_episode(entry):
    try:
        if re.match(r"MBMBaM \d", entry["title"],  re.IGNORECASE):
            num, title = re.search(r"MBMBAM (\d*) ?: (.*)$", entry["title"], re.MULTILINE | re.IGNORECASE).groups()
        elif entry["title"].startswith("My Brother, My Brother and Me "):
            num, title = re.search(r"My Brother, My Brother and Me (\d*): (.*)$", entry["title"], re.MULTILINE | re.IGNORECASE).groups()
        elif entry["title"].startswith("My Brother, My Brother and Me:"):
            num = re.search(r"My Brother, My Brother and Me: Episode (\d*)$", entry["title"], re.MULTILINE | re.IGNORECASE).groups()[0]
            title = f"Episode {num}"
        else:
            num = ""
            title = entry["title"]
    except:
        print(entry["title"])
        return None

    episode = {}
    try:
        num = entry["itunes_episode"]
    except:
        pass
    h, m, s = entry["itunes_duration"].split(":")
    episode["duration"] = int(h)*60*60 + int(m)*60 + int(s)
    episode["published"] = datetime.strptime(entry["published"], '%a, %d %b %Y %H:%M:%S %z')
    episode["title_no_number"] = title
    episode["title"] = entry["title"]
    episode["summary"] = entry["summary"]
    episode["num"] = num
    
    return(episode)

feed = feedparser.parse("https://feeds.simplecast.com/wjQvYtdl")


with open("Data/feed.csv", "w") as f:
    writer = csv.writer(f, delimiter=";")
    header = ["num", "titel", "title_no_number", "summary", "duration", "published"]
    writer.writerow(header)
    for entry in feed.entries:
        episode = prase_episode(entry)
        if episode:
            writer.writerow([
                episode["num"], 
                episode["title"], 
                episode["title_no_number"], 
                episode["summary"],
                episode["duration"], 
                episode["published"]
            ])
