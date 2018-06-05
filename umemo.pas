unit uMemo;
{   A memo class
    Holds the data for a Memo Entry.
    This is adapted from the StickyNote class and used in a similar way..

    This class is manipulated by the memos class.

    The read / write routines where inspired by
    http://www.angelfire.com/hi5/delphizeus/customfiles.html
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lcltype, Graphics;

type
  memo = class

  private
    _name: string;           //  name of the Memo.
    _id  : integer;          //  unique id of the Memo.

    _body   : string;        //  memo text..
    _encrypt: Boolean;       //  Is the Memo encrypted.

    procedure error(Const msg : string);
  public
    property name: string  read _name write _name;
    property id  : integer read _id   write _id;

    property body   : string  read _body    write _body;
    property encrypt: Boolean read _encrypt write _encrypt;

    constructor Create(sn_id: integer); overload;
    constructor Create(fsOut: TFileStream); overload;
    procedure saveToFile(fsOut: TFileStream);

  end;

implementation

constructor memo.Create(sn_id: integer);
{  Creates a new memo.

}
begin
  id := sn_id;

  name    := '';
  body    := '';
  encrypt := false;
end;

constructor memo.Create(fsOut: TFileStream);
{  Creates a new memo.
   A filestream has to be specified, the sticky notes is then created
   from date read from the filestream.

   If the filestream is empty a blank memo is returned.
   An exception is raised if there is an error on write.
}
var
  Len1: Cardinal = 0;
  sNme: string;
  sBdy: string;
  iID : integer = 0;
  bEnc: boolean = true;
begin

  try
    //  need the dummy variables, had the error Can't take the
    //  address of constant expressions..
    fsout.ReadBuffer(Len1, Sizeof(Len1));   //  read name;
    SetLength(sNme, Len1);
    fsOut.ReadBuffer(sNme[1], len1);

    fsout.ReadBuffer(Len1, Sizeof(Len1));   //  read Body;
    SetLength(sBdy, Len1);
    fsOut.ReadBuffer(sBdy[1], len1);

    fsOut.ReadBuffer(iID, sizeof(iID));     //  read id.
    fsOut.ReadBuffer(bEnc, sizeof(bEnc));   //  read encrypt;

    body := sBdy;
    id   := iID;

    name := sNme;
    body := sBdy;
    id   := iID;
    encrypt := bEnc;
  except
    error('Error on Sticky Note Read');
  end;
end;

procedure memo.saveToFile(fsOut: TFileStream);
{  Save a sticky note to the specified filestream.
   An exception is raised if there is an error on write.
}
var
  Len1: Cardinal;
begin
  try
    Len1 := Length(name);
    fsOut.WriteBuffer(Len1, SizeOf(Len1));
    fsOut.WriteBuffer(name[1], Len1);

    Len1 := Length(body);
    fsOut.WriteBuffer(Len1, SizeOf(Len1));
    fsOut.WriteBuffer(body[1], Len1);

    fsOut.WriteBuffer(id, sizeof(id));
    fsOut.WriteBuffer(encrypt, sizeof(encrypt));

  except
    error('Error on Sticky Note Write');
  end;
End;

procedure memo.error(Const msg : string);
{  Raises a custom exception.    }
begin
  raise exception.create(Msg) at
    get_caller_addr(get_frame),
    get_caller_frame(get_frame);
end;


end.


