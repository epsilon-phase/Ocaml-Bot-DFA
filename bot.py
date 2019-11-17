import os
import sys
from mastodon import Mastodon
client=None
base_url=None
username=None
password=None
cw=None
interval=None
post_count=1
with open("passwd.txt") as f:
    for i in f:
        i=i.strip()
        if base_url is None:
            base_url=i
        elif username is None:
            username=i
        elif password is None:
            password=i
        elif cw is None and i.startswith("cw="):
            cw=i.split('=')[1]
        elif i.startswith('interval='):
            interval=int(i.split('=')[1])
        elif i.startswith("count="):
            post_count=int(i.split('=')[1])


if not os.path.exists("secret.txt"):
    Mastodon.create_app("ocaml_dfa_bot",
                        api_base_url=base_url,
                        website="https://lua-mu.org",
                        to_file="secret.txt")
client=Mastodon(api_base_url=base_url,
                client_id="secret.txt")
client.log_in(username,password,to_file="usercred.txt")

def post(msg):
    client.status_post(msg,visibility='unlisted',spoiler_text=cw)

def post_loop(function):
    from datetime import datetime,timedelta
    q=timedelta(minutes=interval)
    d=datetime.now()
    for i in range(count):
        print(d+q*i)
        client.status_post(function(),
                           visibility='unlisted',
                           spoiler_text=cw,
                           scheduled_at=d+q*i)
