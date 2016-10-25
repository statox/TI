Megacar v1.0

by Paul Marks
http://paul248.cjb.net
paul248@myrealbox.com

Platform: TI-83 Plus / Flash Application

Overview:
	This is a port of Jonah Cohen's Megacar from the TI-86.  The point of
	the game is to drive around the track, avoiding walls as much as possible,
	and completing the race in the least amount of time.  You may accelerate,
	brake, and rotate clockwise or counterclockwise.  See Jonah's documentation
	below for a bit more detail.

Controls at Level Selection Screen:
	Left / Right: Select level
	2nd: Start Race
	Mode: Exit
	
In-game Controls:
	2nd: Accelerate
	Alpha: Brake
	Left / Right: Steer counterclockwise/clockwise
	Mode: Quit race.  If you have broken a lap record, it will be recorded.
	Del: Pause
	+/-: Change contrast (while paused)

Entering Initials:
	A-Z: Enter letters
	Del: Backspace
	Enter: Confirm
	Mode: Ignore high score

Creating Levels:
	Levels should be created using Kirk Meyer's level editor for the TI-86.
	I have included a level converter (mc83plc.exe), which converts from
	TI-86 (.86S) levels to TI-83 Plus (.8XP) levels.  If you plan to share
	your new levels publicly, then I recommend that they be distributed
	as .86S files, so they can be used on both the 86 and the 83+.

Notes:
	If you get a new high score on an archived level, that level will be
	unarchived, updated, and rearchived upon switching to another level.
	If you don't like programs messing with your flash, then press Mode
	at the Initials screen, and the new score will be discarded.

	Do not feed invalid levels into the level converter.  I didn't include
	much error checking, so attempting to process an invalid level will
	have unpredictable results.
	
	The app source code is for ZDS 3.68, and the level converter source
	code is for Visual C++ 6.

Version History:
	19Mar2002 - v1.0 - Initial Release

-------------------------------------------------------------------------------
>  megacar.txt from the TI-86 version
-------------------------------------------------------------------------------

Megacar 1.1
by Jonah Cohen <ComAsYuAre@aol.com>
http://jonah.ticalc.org
ICQ UIN: 34197076


About Megacar:
   Megacar is loosely based on the TI-89 game originally made by
   Thomas Fernique (FFernique@eisti.fr).  The objective of the
   game is to get the best racing time possible.  You can accelerate
   and brake, and if you veer off the track onto the grass you will
   automatically slow down.  Braking around turns is recommended...
   If you get the best single lap time or overall race time, you can
   store your initials.  The 8 included levels, "mtracks.86g", were
   converted from the TI-89 TrikTrak levels.  Also, a level editor
   for Windows, written by Kirk Meyer, is included so you can create
   your own.


Controls at Level Selection Screen:
   Left/Right - Select level
   2nd/Enter - Start game
   Exit - Quit

Controls in Game:
   2nd - Accelerate
   Alpha - Brake
   Left/Right - Steer car
   More - Pause (+/- to adjust contrast while paused)
   Exit - Quit


Updates:
   v1.1 (January 6, 2001)
     - Updated tileset to improve tile matching in level editor.
       NOTE: Levels from v1.0 will look very strange in later
       versions of Megacar, so it is recommended that you reload
       the levelset and recreate any levels you have been making.


Thanks to:
   Kirk Meyer
   Matthew Shepcar
   Thomas Fernique
   Ben Mickle
   Danny Confrid