About Klock

Klock - a clock with a k.

A multifunction timing thingy, where some [but not necessary all] of the things are useful.

The multifunction timing thingy is split into many things;

Fuzzy Time, World Klock, Countdown, Timer, Event, Reminder, Memo, Conversion, Sticky Notes & Clipboard Monitor.

Also, the time can be displayed in several ways - text, digital, analogue and binary klocks,


Klock currently consists of over 10,000 lines of Lazarus [Free Pascal] code.  Klock serves as a vehicle by which I learn and tinker with programming.  Previous versions of Klock have existed in VB.net, Python and Free Basic - and may again.

The program starts and finishes with a Splash Screen.  This is really needed if a number of fonts are used.  The starting of Klock lengthens with the number of fonts loaded.

Fuzzy Time

Allows the current time to be displayed in a number of different formats.


World Klock

This display a list of Time Zones and when selected will display the time in that time zone.


Countdown

Implements a simple countdown timer [between 1 and 120 minutes].  
The time for the countdown can be either entered directly of the up/down controls can be used.  The timer can be paused during countdown and stopped before the end is reached.  At the end of the countdown, either a sound can be played, a reminder displayed, a system command [system shut down or re-boot] or a external program can be run - or any combination of the four.


Timer

Implements a simple timer, with split time function.
The timer can be stopped, paused and resumed.
A split function allows for given time to be remembered.


Events

This enables for events to set up and notifications displayed to the user.

Today's date is default, but a future date can also be selected.  A time can also be added to the reminder date.  If no date is added, the reminder will fire at midnight.  When the reminder is due either a sound can be played, a reminder displayed, a system command [system shut down or re-boot] or a external program can be run - or any combination of the four.

Note : Setting the time to 00:00, indicates midnight of the previous day so will not enable the set button.  To set a reminder for midnight of today, disable the time and pick tomorrow's date.  00:00 indicates zero minutes into the present day.


Reminder

Implements a reminder option.
This enables for [recurring] Reminders to set up and reminders displayed to the user.  A number of pre-defined events types are already set up.


Memo

Implements a simple memo thingy.
This will allow memo's to be entered, edited, printed and saved.  Any previously saved memos will be loaded into Klock on start-up.  The memos can be optionally encrypted and only be displayed on demand.


Conversion

Implements a simple conversion screen.
This allows the conversion between various units of weight, distance and area etc.


Stick Notes

Implements a simple sticky Note thingy.  
This allows for the creation of stick Notes, small independent windows that appear on the screen which can hold memo, reminders etc.  Klock will keep track of the notes and re-display any active notes when Klock is first launched.



The button bar contains three buttons -
help  - will display a simple help screen, depending upon the tab being currently viewed [not yet implemented].
Close - Quits the application.
Hide  - Switches the application to the system area.
Also, a quick sticky note launch button.

When the program is running in the system area, the following options are available.
If hovered over, the current time will be displayed.
Right clicked on the icon will display a pop up menu.
This will allow the application to be restored, the application can be exited or e current time can be displayed.

If the application is in the task bar, any display messages
[i.e. a reminder becoming due] will be displayed in a notification window [as will errors etc.].



The status bar contains [after the time & date] the legend cns.
The c represents caps lock.
The n represents Num Lock.
The S represents Scroll Lock.
The letter will be upper case if the key is activated, otherwise lower case.


The application also contains a options screen [file / Options] which allows various options [over sixty] to be set.
NB: If the option is checked to start on windows start up, it only operates for the current user.


Program partially tested on Windows 7 & 10 [both 32 & 64 bit].

