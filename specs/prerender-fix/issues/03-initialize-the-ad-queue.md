# Initialize the ad queue without the Venatus script

Status: resolved

`Client/Components/Ads.js` calls `self.__VM.push(...)` in `createAd`, `removeAd` and `refreshAd`. Only `index.html` defines `self.__VM`; `index.prerender.html` loads no ad scripts, so the prerenderer hits an undefined global and every ad slot throws. Halogen's queue dies with it, which is why the top bar keeps its `Unknown` player status and page components never swap in data they already fetched.

The change is in the working tree: an `adQueue` helper creates `self.__VM` if it is missing and returns it, and the three call sites go through it. Callbacks pushed onto a queue nobody drains never run, so the prerenderer still shows no ads.

Verified by driving the rendertron image's Chrome at the local stack with the rebuilt bundle: `/games/valorant` prerenders 14 KB with the call to action and the title `Valorant Team Finder / LFG / LFT / LFM / LFP | TeamTavern`, `/games/valorant/players` prerenders 45 KB of posts, and no page error is reported. Before the change the same pages serialized to a 5.6 KB shell.

## To do

1. Commit the `Ads.js` change.
2. On the server, pull and rebuild the client:

   ```bash
   ./build-client.sh
   ```

   Caddy serves `dist-client` from a bind mount and the bundle file names are hashed, so nothing needs restarting.

3. Confirm from outside:

   ```bash
   curl -s -A "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)" https://www.teamtavern.net/games/valorant | grep -c call-to-action
   ```

## Done when

The Googlebot request above returns a page with posts and a `<title>` specific to the game.

## Comments

Live on production. All eleven `/games/<handle>` pages prerender around 14 KB with the call to action and their own title, `/games/valorant/players` prerenders 49 KB of posts, and the home page 13.6 KB. Before the two fixes every one of them was the same 5.6 KB shell.
