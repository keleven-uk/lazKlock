unit uEvent;

{   A Event class
    Holds the data for a Event Entry.
    This is adapted from the StickyNote class and used in a similar way..

    This class is manipulated by the Events class.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs;

type
  Event = class

  private
    _name : string;           //  Event name.
    _id   : integer;          //  unique id of the Event.

    _date : string;           //  Event date.
    _time : string;           //  Event time.
    _type : integer;          //  Event type.
    _notes: string;           //  Event notes.
    _float: boolean;          //  shall event be added to floating test?

    _dueShort: string;       //  interval to go to event in days.
    _dueLong : string;       //  interval to go to event in days and time.

    //  if an event becomes due, it will generate a prompt for the user.
    //  There are three prompts that will be generated as the event gets nearer.
    //  If an prompt is acknowledged, these are set to true, so it wont be duplicated.
    _stage1Ack: boolean;     //  stage 1 has been acknowledged.
    _stage2Ack: boolean;     //  stage 2 has been acknowledged.
    _stage3Ack: boolean;     //  stage 3 has been acknowledged.

  procedure error(Const msg : string);
  public
    property name: string  read _name write _name;
    property id  : integer read _id   write _id;

    property date : string  read _date  write _date;
    property time : string  read _time  write _time;
    property etype: integer read _type  write _type;
    property notes: string  read _notes write _notes;
    property float: boolean read _float write _float;

    property dueShort: string  read _dueShort write _dueShort;
    property dueLong : string  read _dueLong  write _dueLong;

    property stage1Ack : boolean  read _stage1Ack  write _stage1Ack;
    property stage2Ack : boolean  read _stage2Ack  write _stage2Ack;
    property stage3Ack : boolean  read _stage3Ack  write _stage3Ack;

    constructor Create(sn_id: integer); overload;
    constructor Create(fsOut: TFileStream); overload;
    procedure saveToFile(fsOut: TFileStream);
    procedure copy(e1: event);
  end;

implementation

constructor Event.Create(sn_id: integer);
{  Creates a new Event.    }
begin
  id := sn_id;

  name  := '';
  date  := '';
  time  := '';
  etype := 0;
  notes := ' ';
  float := false;

  dueShort := '';
  dueLong  := '';

  stage1Ack := false;
  stage2Ack := false;
  stage3Ack := false;
end;

constructor Event.Create(fsOut: TFileStream);
{  Creates a new Event.
   A filestream has to be specified, the Event is then created
   from data read from the filestream.

   If the filestream is empty a blank Event is returned.
   An exception is raised if there is an error on write.

   The boolean fields are converted to and written as strings.
}
var
  len1: Cardinal = 0;
  sNme: string   = '';
  iID : integer  = 0;
  sDte: string   = '';
  sTme: string   = '';
  iTyp: integer  = 0;
  sNte: string   = '';
  sFlt: string   = '';
  sSt1: string   = '';
  sSt2: string   = '';
  sSt3: string   = '';
begin

  try
    //  need the dummy variables, had the error Can't take the
    //  address of constant expressions..
    fsout.ReadBuffer(len1, Sizeof(len1));   //  read name.
    SetLength(sNme, len1);
    fsOut.ReadBuffer(sNme[1], len1);

    fsOut.ReadBuffer(iID, sizeof(iID));     //  read id.

    fsout.ReadBuffer(len1, Sizeof(len1));   //  read date.
    SetLength(sDte, len1);
    fsOut.ReadBuffer(sDte[1], len1);

    fsout.ReadBuffer(len1, Sizeof(len1));   //  read time.
    SetLength(sTme, len1);
    fsOut.ReadBuffer(sTme[1], len1);

    fsOut.ReadBuffer(iTyp, sizeof(iTyp));   //  read type.

    fsout.ReadBuffer(len1, Sizeof(len1));   //  read notes
    SetLength(sNte, len1);
    fsOut.ReadBuffer(sNte[1], len1);

    fsout.ReadBuffer(len1, Sizeof(len1));   //  read floating
    SetLength(sFlt, len1);
    fsOut.ReadBuffer(sFlt[1], len1);

    fsout.ReadBuffer(Len1, Sizeof(len1));   //  read stage 1 acknowledged.
    SetLength(sSt1, Len1);
    fsOut.ReadBuffer(sSt1[1], len1);

    fsout.ReadBuffer(len1, Sizeof(len1));   //  read stage 2 acknowledged.
    SetLength(sSt2, len1);
    fsOut.ReadBuffer(sSt2[1], len1);

    fsout.ReadBuffer(len1, Sizeof(len1));   //  read stage 3 acknowledged.
    SetLength(sSt3, len1);
    fsOut.ReadBuffer(sSt3[1], len1);

    //  don't need to read due interval - they are calculated

    name := sNme;
    id   := iID;

    date  := sDte;
    time  := sTme;
    etype := iTyp;
    notes := sNte;

    float     := StrToBool(sFlt);
    stage1Ack := StrToBool(sSt1);
    stage2Ack := StrToBool(sSt2);
    stage3Ack := StrToBool(sSt3);

    dueShort := '';
    dueLong  := '';
  except
    on E: Exception do
      error('Error on Events Read' + LineEnding + E.Message);
  end;
end;

procedure Event.saveToFile(fsOut: TFileStream);
{  Save a event to the specified filestream.
   An exception is raised if there is an error on write.

   The boolean fields are converted to and written as strings.
}
var
  len1 : Cardinal;
  sFlt : string = '';
  sAck1: string = '';
  sAck2: string = '';
  sAck3: string = '';
begin
  try
    len1 := Length(name);                             //  write name.
    fsOut.WriteBuffer(len1, SizeOf(len1));
    fsOut.WriteBuffer(name[1], len1);

    fsOut.WriteBuffer(id, sizeof(id));                 //  write id.

    len1 := Length(date);                              //  write date.
    fsOut.WriteBuffer(Len1, SizeOf(len1));
    fsOut.WriteBuffer(date[1], len1);

    len1 := Length(time);                              //  write time.
    fsOut.WriteBuffer(Len1, SizeOf(len1));
    fsOut.WriteBuffer(time[1], len1);

    fsOut.WriteBuffer(etype, sizeof(etype));           //  write type.

    len1 := Length(notes);                             //  write notes
    fsOut.WriteBuffer(len1, SizeOf(len1));
    fsOut.WriteBuffer(notes[1], len1);

    sFlt := BoolToStr(float);
    len1 := Length(sFlt);                              //  write floating
    fsOut.WriteBuffer(len1, SizeOf(len1));
    fsOut.WriteBuffer(sFlt[1], len1);

    sAck1 := BoolToStr(stage1Ack);
    len1 := Length(sAck1);                              //  write stage 1 acknowledged.
    fsOut.WriteBuffer(Len1, SizeOf(len1));
    fsOut.WriteBuffer(sAck1[1], len1);

    sAck2 := BoolToStr(stage2Ack);
    len1 := Length(sAck2);                              //  write stage 2 acknowledged.
    fsOut.WriteBuffer(Len1, SizeOf(len1));
    fsOut.WriteBuffer(sAck2[1], len1);

    sAck3 := BoolToStr(stage3Ack);
    len1 := Length(sAck3);                              //  write stage 3 acknowledged.
    fsOut.WriteBuffer(len1, SizeOf(len1));
    fsOut.WriteBuffer(sAck3[1], len1);

    //  don't need to write due interval - they are calculated
  except
    on E: Exception do
      error('Error on Events Write' + LineEnding + E.Message);
  end;
End;

procedure Event.copy(e1: event);
{  Copies the supplies event into the current event.    }
begin
  name      := e1.name;
  id        := e1.id;
  date      := e1.date;
  time      := e1.time;
  etype     := e1.etype;
  notes     := e1.notes;
  float     := e1.float;
  stage1Ack := e1.stage1Ack;
  stage2Ack := e1.stage2Ack;
  stage3Ack := e1.stage3Ack;
  dueShort  := e1.dueShort;
  dueLong   := e1.dueLong;
end;

procedure Event.error(Const msg : string);
{  Raises a custom exception.    }
begin
  raise exception.create(Msg) at
    get_caller_addr(get_frame),
    get_caller_frame(get_frame);
end;

end.

