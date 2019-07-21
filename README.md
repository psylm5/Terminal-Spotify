# Terminal Spotfiy

A terminal application, used to control playback of spotify.

Implemented in Haskell, and built as an extension to [Haskify](https://github.com/sw3dish/haskify).

The libraries Aeson and Wreq are used to interact with the spotify API.

# Instructions for use  

1. Navigate to the root of the project and run `stack build`  
2. Go to https://accounts.spotify.com/authorize?client_id=50458bd0a53d431da383bda6bfcc1b07&response_type=code&redirect_uri=https%3A%2F%2Fexample.com%2Fcallback&scope=user-read-playback-state playlist-modify-private user-modify-playback-state, and login with spotify to allow the application to interact with your player.
3. Upon doing so, you will be redirected to the web page https://exmaple.com/callback?code=[code], where in place of [code], there will be an authorization code. Copy this code.  
4. Carry out the following curl command with your code in place of [code]: `curl -H "Authorization: Basic NTA0NThiZDBhNTNkNDMxZGEzODNiZGE2YmZjYzFiMDc6OGM3ZDVkYWFlZGZhNDNmM2E0ODlkN2FmN2ZmMTE1OTA=" -d grant_type=authorization_code -d code=[code] -d redirect_uri=https%3A%2F%2Fexample.com%2Fcallback https://accounts.spotify.com/api/token`   
5. In the response, there will be a field called "refresh_token". Copy the string into a file, and save it under /app/refresh_token.txt.  
6. From the root of the project, run `stack exec terminal-spotify-exe`  


