unit uEvents;

{  A class to manipulate Events.
   The Event is specified in another class and only used here.
   This is adapted from the StickyNotes class and used in a similar way.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uEvent, fgl, Forms, Dialogs, Graphics, StdCtrls,
  ExtCtrls, LCLType, LCLIntf, Controls, Menus, FileUtil, dateUtils,
  formEvent, uweddingAnniversary, LazFileUtils;

type
  Events = class

  private
    _eventsFile      : string;
    _ScrollEventsFile: string;
    _eventsTypes     : TStringList;
    _eventsCount     : integer;
    _stage1Days      : integer;
    _stage2Days      : integer;
    _stage3Days      : integer;
    _stage1Mess      : string;
    _stage2Mess      : string;
    _stage3Mess      : string;
    _stage1BackColour: TColor;
    _stage2BackColour: TColor;
    _stage3BackColour: TColor;
    _stage1ForeColour: TColor;
    _stage2ForeColour: TColor;
    _stage3ForeColour: TColor;

    property eventsFile:       string read _eventsFile       write _eventsFile;
    property ScrollEventsFile: string read _ScrollEventsFile write _ScrollEventsFile;

    function checkStages(age: integer; e: event): integer;
    procedure actionEvent(pos: integer; stage: integer);
    function determineDueDays(eventDate: string): integer;
    function determineYearsbetween(eventDate: string): integer;
    procedure sortEventsStore;
    procedure swapEvents(f, g: integer);
    procedure addToScolling(f: integer);
    procedure clearScolling;
  public
    property eventsTypes     : TStringList read _eventsTypes;                //  read only.
    property eventsCount     : integer     read _eventsCount      write _eventsCount;
    property stage1Days      : integer     read _stage1Days       write _stage1Days;
    property stage2Days      : integer     read _stage2Days       write _stage2Days;
    property stage3Days      : integer     read _stage3Days       write _stage3Days;
    property stage1Mess      : string      read _stage1Mess       write _stage1Mess;
    property stage2Mess      : string      read _stage2Mess       write _stage2Mess;
    property stage3Mess      : string      read _stage3Mess       write _stage3Mess;
    property stage1BackColour: TColor      read _stage1BackColour write _stage1BackColour;
    property stage2BackColour: TColor      read _stage2BackColour write _stage2BackColour;
    property stage3BackColour: TColor      read _stage3BackColour write _stage3BackColour;
    property stage1ForeColour: TColor      read _stage1ForeColour write _stage1ForeColour;
    property stage2ForeColour: TColor      read _stage2ForeColour write _stage2ForeColour;
    property stage3ForeColour: TColor      read _stage3ForeColour write _stage3ForeColour;

    constructor Create(ef: string; sef: string); overload;
    destructor Destroy; override;

    procedure new(key: string; date: string; time: string; etype: integer; data: string; floating: Boolean);
    procedure amend(id:integer; itmDate: string; itmTime: string; itmType: integer; itmData: string; itmFloat: Boolean);
    function retrieve(id: integer): Event;
    procedure restoreEvents;
    procedure updateEvents;
    procedure saveEvents;
    procedure saveEventsCSV(fileName: string);
    procedure killEvents;
    procedure acknowledgeEvent(pos: integer; stage: integer);
    procedure Remove(pos: integer);
  end;

  //  effectively a dictionary.
  keyStore = specialize TFPGMap<integer, Event>;

//  dimensions of the event prompt form.
const
  LEFT   = 10;
  TOP    = 10;
  WIDTH  = 370;
  HEIGHT = 120;

VAR
  eventsStore: keyStore;
  wg         : WeddingGifts;

implementation

constructor Events.Create(ef: string; sef: string); overload;
{  set up some variables on create.

   File to hole events is held in user options and passed in.
   ef = events file, sef = scrolling events file.
}
begin
  eventsFile         := ef;
  ScrollEventsFile   := sef;
  eventsStore        := keyStore.Create;       //  initial eventStore
  eventsStore.Sorted := true;
  eventsCount        := 0;

  wg := WeddingGifts.Create;

  _eventsTypes := TStringList.Create;
  _eventsTypes.CommaText := ('"Anniversary", "Appointment", "Birthday", "Motor", "Holiday", "Meeting",' +
   '"One Off", "TODO", "Other Time"');

  //  set up defualts, are overridded by user options.
  stage1Days := 5;                             //  Days until stage 1 is triggered.
  stage2Days := 10;                            //  Days until stage 2 is triggered.
  stage3Days := 30;                            //  Days until stage 3 is triggered.

  stage1Mess := ' is realy soon';              //  Display message for stage 1.
  stage2Mess := ' Will very soon be here';     //  Display message for stage 2.
  stage3Mess := ' will soon be here';          //  Display message for stage 3.

  stage1BackColour := clred;                   //  Paper colour of stage 1.
  stage2BackColour := clYellow;                //  Paper colour of stage 2.
  stage3BackColour := clSkyBlue;               //  Paper colour of stage 3.

  stage1ForeColour := clBlack;                 //  Text colour of stage 1.
  stage2ForeColour := clBlack;                 //  Text colour of stage 2.
  stage3ForeColour := clBlack;                 //  Text colour of stage 3.
end;

destructor Events.Destroy;
{  run on destroy.    }
begin
  eventsStore.free;
  _eventsTypes.free;

  wg.free;

  inherited;
end;

procedure Events.new(key: string; date: string; time: string; etype: integer; data: string; floating: Boolean);
{  Creates a new Event.  This is called from the host program.
   A new Events is created and added to the store.
   The Events store is then saved to file.
}
VAR
  e: Event;                   //  Event.
begin
  eventsCount := eventsCount + 1;

  e := Event.Create(eventsCount);

  if data = '' then data := ' ';   // Don't allow a blank notes.

  e.name      := key;
  e.id        := eventsCount;
  e.date      := date;
  e.time      := time;
  e.etype     := etype;
  e.notes     := data;
  e.float     := floating; //  If true, add to scrolling text.
  e.stage1Ack := false;    //  if an event becomes due, it will generate a prompt for the user.
  e.stage2Ack := false;    //  There are three prompts that will be generated as the event gets nearer.
  e.stage3Ack := false;    //  If an prompt is acknowledged, these are set to true, so it won't be duplicated.
  e.dueShort  := '';
  e.dueLong   := '';

  eventsStore.Add(eventsCount, e);

  if floating then addToScolling(eventsCount - 1);  //  If needed, add to text file of scrolling text.
                                                    //  Need to subtract 1, event store is zzero based.
  saveEvents;
end;

procedure Events.amend(id:integer; itmDate: string; itmTime: string; itmType: integer;
                                                                         itmData: string; itmFloat: Boolean);
{  Amends an event - the date and body text can be amended.
   The event store is then saved to file.
}
begin
  if itmData = '' then itmData := ' ';   // Don't allow a blank notes.

  eventsStore.Data[id].date  := itmDate;
  eventsStore.Data[id].time  := itmTime;
  eventsStore.Data[id].etype := itmType;
  eventsStore.Data[id].notes := itmData;
  eventsStore.Data[id].float := itmFloat;

  updateEvents;
end;

function Events.retrieve(id: integer): Event;
{returns a Event out of the store at position id.    }
VAR
  e: Event;                   //  Events.
begin
  e           := Event.Create(0);
  e.name      := eventsStore.Data[id].name;
  e.id        := eventsStore.Data[id].id;
  e.date      := eventsStore.Data[id].date;
  e.time      := eventsStore.Data[id].time;
  e.etype     := eventsStore.Data[id].etype;
  e.notes     := eventsStore.Data[id].notes;
  e.float     := eventsStore.Data[id].float;
  e.stage1Ack := eventsStore.Data[id].stage1Ack;
  e.stage2Ack := eventsStore.Data[id].stage2Ack;
  e.stage3Ack := eventsStore.Data[id].stage3Ack;
  e.dueShort  := eventsStore.Data[id].dueShort;
  e.dueLong   := eventsStore.Data[id].dueLong;

  result := e
end;

procedure Events.Remove(pos: integer);
{  Remove a Event from the store at a given position.

   pos comes from the combo box and is zero based,
   the event store starts at one.
}
begin
  eventsStore.Remove(pos + 1);
  eventsCount := EventsCount - 1;
  saveEvents;
end;

procedure Events.saveEvents;
{  Write out the contents of the Event Store to a binary file.    }
var
  fileOut: TFileStream;
  f      : integer;
begin
  if eventsCount = 0 then
  begin
    DeleteFile(eventsFile);    //  Events file is empty, delete if there.
    exit;
  end;

  fileOut := TFileStream.Create(eventsFile, fmCreate or fmShareDenyWrite);

  try
    for f := 0 to eventsCount - 1 do
    begin
      try
        eventsStore.Data[f].saveToFile(fileOut);
      except
        on E: EInOutError do
      end;
    end;
  finally
    fileOut.Free;
  end;
end;

procedure Events.saveEventsCSV(fileName: string);
{  Write out the contents of the Event Store to a CSV file.    }
VAR
  txtFile   : TextFile;
  csvText   : string;
  addToFloat: string;
  ack1      : string;
  ack2      : string;
  ack3      : string;
  f         : integer;
begin
  AssignFile(txtFile, fileName);

  try
    rewrite(txtFile);           //  Always start afresh.

    csvText := 'Name, Date, time, Id, Type, Notes, Add to Float';    //  Add header
    writeLn(txtFile, csvText);

    for f := 0 to eventsCount - 1 do
    begin
      if eventsStore.Data[f].float then
        addToFloat := 'True'
      else
        addToFloat := 'False';

      if eventsStore.Data[f].stage1Ack then
        ack1 := 'True'
      else
        ack1 := 'False';

      if eventsStore.Data[f].stage2Ack then
        ack2 := 'True'
      else
        ack2 := 'False';

      if eventsStore.Data[f].stage3Ack then
        ack3 := 'True'
      else
        ack3 := 'False';

      csvText := format('%s, %d, %s, %s, %d, %s, %s, %s, %s, %s', [eventsStore.Data[f].name,
                                                                   eventsStore.Data[f].id,
                                                                   eventsStore.Data[f].date,
                                                                   eventsStore.Data[f].time,
                                                                   eventsStore.Data[f].etype,
                                                                   eventsStore.Data[f].notes,
                                                                   addToFloat, ack1, ack2, ack3]);

      writeLn(txtFile, csvText);
    end;

  finally
    CloseFile(txtFile);
  end;
end;

procedure Events.restoreEvents;
{  Read in a binary file and convert contents to events and populate the store.
}
var
  fileIn: TFileStream;
begin
  eventsCount := 0;

  if not fileExists(eventsFile) then exit;       //  file not there then exit
  if fileSize(eventsFile) = 0   then exit;       //  empty file then exit.

  fileIn := TFileStream.Create(eventsFile, fmOpenRead or fmShareDenyWrite);

  try
    repeat
      try
        eventsStore.Add(eventsCount, Event.Create(fileIn));
        eventsCount := eventsCount + 1;
        except
          on E: EInOutError do
          ShowMessage('ERROR : Reading Event file');
      end;  //  try
      until FileIn.Position >= fileIn.Size;
   finally
    fileIn.Free;
  end;        //  try
end;

procedure Events.updateEvents;
{  For each event, calculate the due interval.
   dueInterval = due interval in days.

   If the event is a birthday or wedding anniversary, then the original year should
   of been used - so we substitute the current year to calculate the days due.
   If the event has already happened, then increment the year.

   todo :: needs to check for event type i.e. if type of motor is passed then the days due should be negative.
}
var
  f      : integer;
  dueDays: integer = 0;
  stage  : integer = 0;
begin
  if eventsCount = 0 then exit;

  clearScolling;     //  so we start with a clear text file each time.

  for f := 0 to eventsCount - 1 do
  begin
    dueDays := determineDueDays(eventsStore.Data[f].date);
    stage   := checkStages(dueDays, eventsStore.Data[f]);
    if stage <> 0 then actionEvent(f, stage);

    if dueDays = 0 then                                                //  event must be today.
      eventsStore.Data[f].dueShort := format('TODAY %s', [EventsStore.Data[f].name])
    else
      eventsStore.Data[f].dueShort := format('%.3d days %s ', [dueDays, EventsStore.Data[f].name]);

    if eventsStore.Data[f].float then           //  if needed, add to scrolling.
      addToScolling(f);

  end;  //  for f := 0 to EventsCount - 1 do

  sortEventsStore;   //  Sort events by due date.
  saveEvents;        //  re save sorted events.
end;

function Events.checkStages(age: integer; e: event): integer;
{  Checks if the days due falls between the age bands, if it does the return
   the number of the stage.  If the event is not due, then return 0.
}
var
  rtnValue: integer = 0;
begin
  if not(e.stage1Ack) and (age < stage1Days) then rtnValue := 1;
  if not(e.stage2Ack) and (age < stage2Days) then rtnValue := 2;
  if not(e.stage3Ack) and (age < stage3Days) then rtnValue := 3;
  result := rtnValue;
end;

procedure Events.clearScolling;
{  Clears the text file that contains the events to add to the scrolling text.  }
begin
  if FileExists(ScrollEventsFile) then DeleteFile(ScrollEventsFile);
end;

procedure Events.addToScolling(f: integer);
{  Will add a specified event to the list of events to be added to the
   scrolling text.
}
VAR
  eventText: string;
  txtFile  : TextFile;
begin
  AssignFile(txtFile, ScrollEventsFile);

  try
    if FileExists(ScrollEventsFile) then
      append(txtFile)
    else
      rewrite(txtFile);

    eventText := format('%s, %s, %s', [eventsStore.Data[f].Name, eventsStore.Data[f].date, eventsStore.Data[f].time]);
    writeLn(txtFile, eventText);
  finally
    CloseFile(txtFile);
  end;
end;

procedure Events.sortEventsStore;
{  Sorts the event store - by a simple bubble sort.
   If the data set gets large, may need a better sort routine.

   First the entry id is updated with the current due days.
   Secondly the store is sorted using the due days [lowest first].
}
var
  f, g: integer;
begin

  for f := 0 to eventsCount - 1 do            //  update due days.
    eventsStore.Data[f].id := determineDueDays(eventsStore.Data[f].date);

  for f := 0 to eventsCount - 1 do            //  perform sort.
  begin
    for g := f + 1 to eventsCount - 1 do
    begin
      if eventsStore.Data[f].id > eventsStore.Data[g].id then swapEvents(f, g);
    end;  //  for g := f + 1 to eventsCount - 1 do
  end;    //  for f := 0 to eventsCount - 1 do
end;

procedure Events.swapEvents(f, g: integer);
{  Swap over two entries in the events store.    }
VAR
  e: Event;
begin
  e := Event.Create(0);
  e.copy(eventsStore.Data[f]);
  eventsStore.Data[f].copy(eventsStore.Data[g]);
  eventsStore.Data[g].copy(e);
  e.Free;
end;

function Events.determineDueDays(eventDate: string): integer;
{  Returns the number of days the event is due, event date is passed in.

   If the event date is in the future [current or future year] then return days difference.
   If the event date is in the past [previous year] then substitute current year and return days difference.

}
VAR
  evntDate: TDateTime;
  sbsDate : TDateTime;
  dueDays : integer = 0;
begin
  evntDate := StrToDate(eventDate);

  if evntDate > Today then                                        //  an upcoming event
    dueDays := DaysBetween(Today, evntDate);

  if evntDate < Today then                                        //  an event with an original year.
    begin
      sbsDate := RecodeYear(evntDate, YearOf(Today));             //  substitute current year.
      if sbsDate < Today then sbsDate := Incyear(sbsDate);        //  is event before today? Then inc year
      dueDays := DaysBetween(Today, sbsDate);                     //  days due.
    end;

  result := dueDays
end;

procedure Events.actionEvent(pos: integer; stage: integer);
{  Inform user of impending event.    }
VAR
  mess: string = '';
  fcol: TColor = clBlack;                           //  Fore colour - colour of font.
  bcol: TColor = clBlue;                            //  back colour - colour of the paper [form].
  ev  : TfrmEvent;                                  //  ev = event Form
  lb  : TLabel;
  yr  : integer;

begin
  case stage of
    1:
    begin
      if eventsStore.Data[pos].stage1Ack then exit;  //  if already acknowledged then exit.
      mess := eventsStore.Data[pos].name + ' ' + stage1Mess;
      bcol := stage1BackColour;
      fcol := stage1ForeColour;
    end;
    2:
    begin
      if eventsStore.Data[pos].stage2Ack then exit;  //  if already acknowledged then exit.
      mess := eventsStore.Data[pos].name + ' ' + stage2Mess;
      bcol := stage2BackColour;
      fcol := stage3ForeColour;
    end;
    3:
    begin
      if eventsStore.Data[pos].stage3Ack then exit;  //  if already acknowledged then exit.
      mess := eventsStore.Data[pos].name + ' ' + stage3Mess;
      bcol := stage3BackColour;
      fcol := stage3ForeColour;
    end;
  end;  //  case stage of

  ev := TfrmEvent.Create(nil);

  ev.SetBounds(LEFT, TOP + (pos * HEIGHT), WIDTH, HEIGHT);
  ev.Name       := format('frmEvent_%d', [eventsStore.Data[pos].id]);
  ev.Color      := bcol;
  ev.Font.Color := fcol;
  ev.AlphaBlend := true;

  ev.pos   := pos;                       //  pass to the form,
  ev.stage := stage;                     //  so the form is aware which event to acknowledge.

  lb            := ev.FindChildControl('lblEvent') as TLabel;
  lb.Font.Color := fcol;
  lb.Caption    := mess;

  if eventsStore.Data[pos].etype = 2 then          //  if a birthday, determine age.
  begin
    yr := determineYearsbetween(eventsStore.Data[pos].date);
    mess    := format('and will be %d years old.', [yr])
  end
  else
    mess    := '';

  lb            := ev.FindChildControl('lblInfo') as TLabel;
  lb.Font.Color := fcol;
  lb.Caption    := mess;

  ev.show
end;

function Events.determineYearsbetween(eventDate: string): integer;
{  Returns the number of years between a given date and today.    }
VAR
  evntDate: TDateTime;
  years   : integer = 0;
begin
  evntDate := StrToDate(eventDate);
  years    := YearsBetween(Today, evntDate);

  result := years;
end;

procedure Events.killEvents;
{  When Klock closes, kill all displayed events if any.
   Tries to find a form for all events - most will fail.  Clumsy I know.
}
VAR
  f : integer;
  s : string;
  ev: TCustomForm;
begin
  if EventsCount = 0 then exit;

  for f := 0 to EventsCount - 1 do
  begin
    try
      s  := format('frmEvent_%d', [eventsStore.Data[f].id]);
      ev := screen.FindForm(s);
    except
                     //  event form not found.
      continue;      //  continue to next event form, if any.
    end;

    if assigned(ev) then ev.Close;
  end;
end;

procedure Events.acknowledgeEvent(pos: integer; stage: integer);
{  The event has been acknowledged, so set the appropriate flag to true.

   This is called from from the event form.
}
begin
  case stage of
    1: eventsStore.Data[pos].stage1Ack := true;
    2: eventsStore.Data[pos].stage2Ack := true;
    3: eventsStore.Data[pos].stage3Ack := true;
  end;

  saveEvents; //  event might of been acknowledged, don't check - just save.
end;

end.

