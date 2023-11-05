# MID2OPL2
Midi to OPL2 VGM Converter (Fortran)

<p align = "justify">The project is in a really early state, the goal is to create a standard midi >> OPL2 VGM converter that has as much Fortran code as possible. Unfortunately, the official Intel interface for accessing Windows API did not work, so those are reached by a little Python executable also made by me.</p>

<B><U>Reladted Projects:</U></B>
<UL> 
  <LI>Fortari 2600 (the main project for Atari 2600 game development): https://github.com/MemberA2600/Fortari2600</LI>
  <LI>OPL2LPT VGM Player (A Windows player for using the OPL2LPT sound card to play VMG files): https://github.com/MemberA2600/OPL2LPTVGMPlayer</LI>
</UL>

Here are some conversion made with this tool: 
<b><a href="https://youtube.com/playlist?list=PLRiYYfr6zccx_YlmFaOYpOitg6AWhjBQH&si=7DNugwhHKsy1vTwk">Click here</a></b>

<p align = "justify">Soundbanks are not made by me! You can find a really cool OPL3 bank editor <b><a href="[https://youtube.com/playlist?list=PLRiYYfr6zccx_YlmFaOYpOitg6AWhjBQH&si=7DNugwhHKsy1vTwk](https://github.com/Wohlstand/OPL3BankEditor/)https://github.com/Wohlstand/OPL3BankEditor/">here</a></b></p>

<p align = "justify">The software let's you dump the midi data, soyou can get information about what it has inside (mostly the meta and system exclusive messages and midi data), and also prints out which notes were used and left out. Because some soundbanks sets the percussion volume too high, you can normalize it, and also limit the polyphony.</p>

<p align = "justify">Feel free to write me (feher.janos.zoltan@gmail.com) if it fails on midi load or vgm conversion, I have checked dosens of midis, but since it's a really messy format, there could be exceptions in the wild! </p>
<p align = "justify">Good source for <b><a href="https://midistock.ru/">midis</a></b>.</p>

<p align = "justify">Development machine: <i>Ryzen 5 2600, 32GB RAM, Windows 7, Visual Studio 2013</i> </p>

<img src = "https://i.ibb.co/b73xkvp/000.png">

