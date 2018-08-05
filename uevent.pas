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
    _type : integer;          //  Event type.
    _notes: string;           //  Event notes.
    _float: boolean;          //  shall event be added to floating test?

    _dueShort: string;       //  interval to go to event in days.
    _dueLong : string;       //  interval to go to event in days and time.

    //  if an event becomes due, it will generate a prompt for the user.
    //  There are three prompts that will be generated as the event gets nearer.
    //  If an prompt is acknowledged, these are set to true, so it wont be duplicated.
    _stage1Ack: boolean;     //  stage 1 has been acknowledged.
    _stage2Ack: boolean;     //  stage 1 has been acknowledged.
    _stage3Ack: boolean;     //  stage 1 has been acknowledged.

  procedure error(Const msg : string);
  public
    property name: string  read _name write _name;
    property id  : integer read _id   write _id;

    property date : string  read _date  write _date;
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
  end;

implementation

constructor Event.Create(sn_id: integer);
{  Creates a new Event.    }
begin
  id := sn_id;

  name  := '';
  date  := '';
  etype := 0;
  notes := '';
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
   from date read from the filestream.

   If the filestream is empty a blank Event is returned.
   An exception is raised if there is an error on write.
}
var
  Len1: Cardinal = 0;
  sNme: string   = '';
  iID : integer  = 0;
  sDte: string   = '';
  iTyp: integer  = 0;
  sNte: string   = '';
  bFlt: boolean  = false;
  bSt1: boolean  = false;
  bSt2: boolean  = false;
  bSt3: boolean  = false;
begin

  try
    //  need the dummy variables, had the error Can't take the
    //  address of constant expressions..
    fsout.ReadBuffer(Len1, Sizeof(Len1));   //  read name.
    SetLength(sNme, Len1);
    fsOut.ReadBuffer(sNme[1], len1);

    fsOut.ReadBuffer(iID, sizeof(iID));     //  read id.

    fsout.ReadBuffer(Len1, Sizeof(Len1));   //  read date.
    SetLength(sDte, Len1);
    fsOut.ReadBuffer(sDte[1], len1);

    fsOut.ReadBuffer(iTyp, sizeof(iTyp));   //  read type.

    fsout.ReadBuffer(Len1, Sizeof(Len1));   //  read notes
    SetLength(sNte, Len1);
    fsOut.ReadBuffer(sNte[1], len1);

    fsOut.ReadBuffer(bFlt, sizeof(bFlt));   //  read floating.

    fsOut.ReadBuffer(bSt1, sizeof(bSt1));   //  read stage 1 acknowledged.
    fsOut.ReadBuffer(bSt2, sizeof(bSt2));   //  read stage 1 acknowledged.
    fsOut.ReadBuffer(bSt3, sizeof(bSt3));   //  read stage 1 acknowledged.

    //  don't need to read due interval - they are calculated

    name := sNme;
    id   := iID;

    date  := sDte;
    etype := iTyp;
    notes := sNte;
    float := bFlt;

    stage1Ack := bSt1;
    stage2Ack := bSt2;
    stage3Ack := bSt3;

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
}
var
  Len1: Cardinal;
begin
  try
    Len1 := Length(name);                             //  write name.
    fsOut.WriteBuffer(Len1, SizeOf(Len1));
    fsOut.WriteBuffer(name[1], Len1);

    fsOut.WriteBuffer(id, sizeof(id));                 //  write id.

    Len1 := Length(date);                              //  write date.
    fsOut.WriteBuffer(Len1, SizeOf(Len1));
    fsOut.WriteBuffer(date[1], Len1);

    fsOut.WriteBuffer(etype, sizeof(etype));           //  write type.

    Len1 := Length(notes);                             //  write notes
    fsOut.WriteBuffer(Len1, SizeOf(Len1));
    fsOut.WriteBuffer(notes[1], Len1);

    fsOut.WriteBuffer(float, sizeof(float));           //  write type.

    fsOut.WriteBuffer(stage1Ack, sizeof(stage1Ack));   //  write stage 1 acknowledged.
    fsOut.WriteBuffer(stage2Ack, sizeof(stage2Ack));   //  write stage 1 acknowledged.
    fsOut.WriteBuffer(stage3Ack, sizeof(stage3Ack));   //  write stage 1 acknowledged.

    //  don't need to write due interval - thay are calculated
  except
    on E: Exception do
      error('Error on Events Write' + LineEnding + E.Message);
  end;
End;

procedure Event.error(Const msg : string);
{  Raises a custom exception.    }
begin
  raise exception.create(Msg) at
    get_caller_addr(get_frame),
    get_caller_frame(get_frame);
end;

end.

