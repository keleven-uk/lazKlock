About Klock

Klock - a clock with a k.

A multifunction timing thingy, where some [but not necessary all] of the things are useful.

The multifunction timing thingy is split into twelve things;

Fuzzy Time, World Klock, Countdown, Timer, Reminder.

Klock currently consists of over 1000 lines of Pascal[Lazarus] code.  Klock serves as a vehicle by which I learn and tinker with programming.  
Previous versions of Klock have existed in VB.net, Python, Pascal [Lazarus] and Freebasic - and may again.

Fuzzy Time

Allows the current time to be displayed in a number of different formats.

NB: The time can be displayed in two different forms of hex time.
Hex time is just current [local] time display in hexadecimal format
Where True Hex Time, divides the day into 16 hours of 256 minutes of 16 seconds.

Countdown

Implements a simple countdown timer [between 1 and 90 minutes].
The time for the countdown can be either entered directly of the up/down controls can be used.  The timer can be paused during countdown and stopped before the end is reached.  At the end of the countdown, either a sound can be played, a reminder displayed, a system command [system shut down or re-boot] or a external program can be run - or any combination of the four.


Timer

Implements  a simple timer, with split time function.
The timer can be stopped, paused and resumed.
A split function allows for given time to be remembered.


Reminder

Implements a reminder option.
Today's date is default, but a future date can also be selected.  A time can also be added to the reminder date.  If no date is added, the reminder will fire at midnight.  When the reminder is due either a sound can be played, a reminder displayed, a system command [system shut down or re-boot] or a external program can be run - or any combination of the four.


Events

Implements a events option.
This enables for [recurring] events to set up and reminders displayed to the user.  A number of pre-defined events types are already set up.


The button bar contains three buttons -
help  - will display a simple help screen, depending upon the tab being currently viewed [not yet implemented].
Close - Quits the application.
Hide  - Switches the application to the system area.


When the program is running in the system area, the following options are available.
If hovered over, the current time will be displayed.
Right clicked on the icon will display a pop up menu.
This will allow the application to be restored, the option screen to be displayed, the application can be exited or toggle the displaying of time in a notification window.

If the application is in the task bar, any display messages
[i.e. a reminder becoming due] will be displayed in a notification window [as will errors etc.].



The status bar contains [after the time & date] the legend cns.
The c represents caps lock.
The n represents Num Lock.
The S represents Scroll Lock.
The letter will be upper case if the key is activated, otherwise lower case.
If the PC is detected to be running on battery, these will appear in red.


The application also contains a options screen [file / Options] which allows various options to be set.
NB: If the option is checked to start on windows start up, it only operates for the current user.


Program partially tested on Windows 7 & 10 [both 32 & 64 bit].

