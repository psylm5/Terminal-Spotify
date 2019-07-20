# Instructions for use  

1. Navigate to the root of the project and run `stack build`  
2. Read the note below on steps 3-6.  
3. Go to "https://accounts.spotify.com/authorize?client_id=50458bd0a53d431da383bda6bfcc1b07&response_type=code&redirect_uri=https%3A%2F%2Fexample.com%2Fcallback&scope=user-read-playback-state playlist-modify-private user-modify-playback-state", and login with spotify to allow the application to interact with your player.
4. Upon doing so, you will be redirected to the web page "https://exmaple.com/callback?code=[code]", where in place of [code], there will be an authorization code. Copy this code.  
5. Carry out the following curl command with your code in place of [code]: `curl -H "Authorization: Basic NTA0NThiZDBhNTNkNDMxZGEzODNiZGE2YmZjYzFiMDc6OGM3ZDVkYWFlZGZhNDNmM2E0ODlkN2FmN2ZmMTE1OTA=" -d grant_type=authorization_code -d code=[code] -d redirect_uri=https%3A%2F%2Fexample.com%2Fcallback https://accounts.spotify.com/api/token`   
6. In the response, there will be a field called "refresh_token". Copy the string into a file, and save it under /app/refresh_token.txt.  
7. From the root of the project, run `stack exec terminal-spotify-exe`  

## Note about steps 3-6  

These steps are explained in detail at ################. The steps are carried out to obatain a refresh_token, needed for the application to gain permission to modify a user's player state as explained in the report. A spotify premium account is required to carry out these steps. I have included a refresh_token for my account in the file /app/refresh_token, however it will not work if I am not playing spotify at the time the application is tested. If you intend to use my refresh_token, then skip to step 7.  

# Source Code Hierarchy

.
├── app
│   ├── Main.hs
│   └── refresh_token.txt
├── haskify.cabal
├── Setup.hs
├── src
│   ├── Haskify.hs
│   └── Types.hs
└── stack.yaml 

## app
Contains the command line spotify player application, written from scratch, and the file where refresh_token resides.

## haskify.cabal, Setup.hs, stack.yaml
Files relating to stack

## src
The haskify library and related types, taken from https://github.com/sw3dish/haskify.  
The following functions have been added to src/Haskify.hs: haskifyPutEndpoint, haskifyPostEndpoint, refreshToken, skipPlayback, resumePlayback, pausePlayback, getCurrentPlayback.  
The CurrentPlayback type and corresponding instance of FromJSON has been added to src/Types.html.  
These code in these files was written, closely following the existing haskify source code. Anything not mentioned was already a part of haskify.
