unit uEvent;
{   A Event class
    Holds the data for a Event Entry.
    This is adapted from the StickyNote class and used in a similar way..

    This class is manipulated by the Events class.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  Event = class

  private
    _name : string;           //  Event name.
    _id   : integer;          //  unique id of the Event.

    _date : string;           //  Event date.
    _type : integer;          //  Event type.
    _notes: string;           //  Event notes.

  procedure error(Const msg : string);
  public
    property name: string  read _name write _name;
    property id  : integer read _id   write _id;

    property date : string  read _date  write _date;
    property etype: integer read _type  write _type;
    property notes: string  read _notes write _notes;

    constructor Create(sn_id: integer); overload;
    constructor Create(fsOut: TFileStream); overload;
    procedure saveToFile(fsOut: TFileStream);
  end;

implementation

constructor Event.Create(sn_id: integer);
{  Creates a new Event.

}
begin
  id := sn_id;

  name  := '';
  date  := '';
  etype := 0;
  notes := ''
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
  sNme: string;
  iID : integer = 0;
  sDte: string;
  iTyp: integer = 0;
  sNte: string;
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

    fsOut.ReadBuffer(iTyp, sizeof(iID));     //  read type.

    fsout.ReadBuffer(Len1, Sizeof(Len1));    //  read notes
    SetLength(sNte, Len1);
    fsOut.ReadBuffer(sNte[1], len1);

    name := sNme;
    id   := iID;

    date  := sDte;
    etype := iTyp;
    notes := sNte;
  except
    error('Error on Sticky Note Read');
  end;
end;

procedure Event.saveToFile(fsOut: TFileStream);
{  Save a sticky note to the specified filestream.
   An exception is raised if there is an error on write.
}
var
  Len1: Cardinal;
begin
  try
    Len1 := Length(name);                      //  write name.
    fsOut.WriteBuffer(Len1, SizeOf(Len1));
    fsOut.WriteBuffer(name[1], Len1);

    fsOut.WriteBuffer(id, sizeof(id));         //  write id.

    Len1 := Length(date);                      //  write date.
    fsOut.WriteBuffer(Len1, SizeOf(Len1));
    fsOut.WriteBuffer(date[1], Len1);

    fsOut.WriteBuffer(id, sizeof(etype));      //  write type.

    Len1 := Length(notes);                     //  write notes
    fsOut.WriteBuffer(Len1, SizeOf(Len1));
    fsOut.WriteBuffer(notes[1], Len1);
  except
    error('Error on Sticky Note Write');
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

