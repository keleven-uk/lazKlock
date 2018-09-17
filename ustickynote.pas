unit ustickyNote;

{   A Sticky Note class
    Holds the data for a single sticky note.

    This class is manipulated by the StickyNotes class.

    The read / write routines where inspired by
    http://www.angelfire.com/hi5/delphizeus/customfiles.html
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lcltype, Graphics;

type
  stickyNote = class

  private
    _body: string;             //  Body text of the Sticky Note.
    _id  : integer;            //  unique id of the Sticky Note.

    _height : integer;         //  Height of the Sticky Note.
    _left   : integer;         //  Left of the Sticky Note.
    _top    : integer;         //  Top of the Sticky Note.
    _width  : integer;         //  Width of the Sticky Note.
    _visable: Boolean;         //  true id the Sticky note is visible on the screen.
    _colour : TColor;          //  colour of the Sticky Note.
    _font   : TFont;           //  font of the Sticky Note.

    procedure error(Const msg : string);
    function FontToString(f: TFont): string;
    function StringToFont(s: string): TFont;
  public
    property body: string read _body write _body;
    property id: integer read _id write _id;

    property height : integer read _height  write _height;
    property left   : integer read _left    write _left;
    property top    : integer read _top     write _top;
    property width  : integer read _width   write _width;
    property visable: Boolean read _visable write _visable;
    property colour : Tcolor  read _colour  write _colour;
    property font   : TFont   read _font    write _font;

    constructor Create(sn_id: integer); overload;
    constructor Create(fsOut: TFileStream); overload;
    procedure saveToFile(fsOut: TFileStream);

  end;

implementation

constructor stickyNote.Create(sn_id: integer);
{  Creates a new stick note.
   The header, body and id of the sticky note has to be specified.
}
begin
  id := sn_id;

  body    := '';
  height  := 0;
  left    := 0;
  top     := 0;
  width   := 0;
  visable := true;
  colour  := clYellow;

  font := TFont.Create;
end;

constructor stickyNote.Create(fsOut: TFileStream);
{  Creates a new stick note.
   A filestream has to be specified, the sticky notes is then created
   from date read from the filestream.

   If the filestream is empty a blank stick Note is returned.
   An exception is raised if there is an error on write.
}
var
  Len1: Cardinal = 0;
  sBdy: string;
  iID : integer = 0;
  iHgt: integer = 0;
  iLft: integer = 0;
  iTop: integer = 0;
  iWdh: integer = 0;
  bVsb: boolean = true;
  cClr: TColor;
  sFnt: string;
begin

  try
    //  need the dummy variables, had the error Can't take the
    //  address of constant expressions..
    fsout.ReadBuffer(Len1, Sizeof(Len1));   //  read body;
    SetLength(sBdy, Len1);
    fsOut.ReadBuffer(sBdy[1], len1);

    fsOut.ReadBuffer(iID, sizeof(iID));     //  read id.

    fsOut.ReadBuffer(iHgt, sizeof(iHgt));   //  read height;
    fsOut.ReadBuffer(iLft, sizeof(iLft));   //  read left
    fsOut.ReadBuffer(iTop, sizeof(iTop));   //  read top;
    fsOut.ReadBuffer(iWdh, sizeof(iWdh));   //  read width;
    fsOut.ReadBuffer(bVsb, sizeof(bVsb));   //  read visible;
    fsOut.ReadBuffer(cClr, sizeof(cClr));   //  read visible;

    fsout.ReadBuffer(Len1, Sizeof(Len1));   //  read header;
    SetLength(sFnt, Len1);
    fsOut.ReadBuffer(sFnt[1], len1);

    body := sBdy;
    id   := iID;

    height  := iHgt;
    left    := iLft;
    top     := iTop;
    width   := iWdh;
    visable := bVsb;
    colour  := cClr;
    font    := StringToFont(sFnt);
  except
    on E: Exception do
      error('Error on Sticky Note Read' + LineEnding + E.Message);
  end;
end;

procedure stickyNote.saveToFile(fsOut: TFileStream);
{  Save a sticky note to the specified filestream.
   An exception is raised if there is an error on write.
}
var
  Len1: Cardinal;
  sFnt: string;
begin
  try
    Len1 := Length(body);
    fsOut.WriteBuffer(Len1,    SizeOf(Len1));
    fsOut.WriteBuffer(body[1], Len1);

    fsOut.WriteBuffer(id,      sizeof(id));

    fsOut.WriteBuffer(height,  sizeof(height));
    fsOut.WriteBuffer(left,    sizeof(left));
    fsOut.WriteBuffer(top,     sizeof(top));
    fsOut.WriteBuffer(width,   sizeof(width));
    fsOut.WriteBuffer(visable, sizeof(visable));
    fsOut.WriteBuffer(colour,  sizeof(colour));

    sFnt := FontToString(font);
    Len1 := Length(sFnt);
    fsOut.WriteBuffer(Len1,    SizeOf(Len1));
    fsOut.WriteBuffer(sFnt[1], Len1);
  except
    on E: Exception do
      error('Error on Sticky Note Write' + LineEnding + E.Message);
  end;
End;

procedure stickyNote.error(Const msg : string);
{  Raises a custom exception.    }
begin
  raise exception.create(Msg) at
    get_caller_addr(get_frame),
    get_caller_frame(get_frame);
end;

function stickyNote.FontToString(f: TFont): string;
{  Produces a string representation of a given font.
   All the font attributes are converted to strings and packed together.

   Does not yet handle errors - will there be any ;o)                                              }
var
  rtnStr: string;
begin
  rtnStr := '';
  rtnStr := rtnStr + IntToStr(f.Charset) + ':';
  rtnStr := rtnStr + ColorToString(f.Color) + ':';
  rtnStr := rtnStr + IntToStr(f.Height) + ':';
  rtnStr := rtnStr + (f.Name) + ':';
  rtnStr := rtnStr + IntToStr(f.Orientation) + ':';
  rtnStr := rtnStr + IntToStr(f.size) + ':';
  rtnStr := rtnStr + IntToStr(Ord(f.Pitch)) + ':';
  rtnStr := rtnStr + IntToStr(Ord(f.Quality)) + ':';

  //                             TFontStyles is a set and scanned differently.
  if fsBold in f.Style then
    rtnStr := rtnStr + 'B'
  else
    rtnStr := rtnStr + '.';

  if fsItalic in f.Style then
    rtnStr := rtnStr + 'I'
  else
    rtnStr := rtnStr + '.';

  if fsStrikeOut in f.Style then
    rtnStr := rtnStr + 'S'
  else
    rtnStr := rtnStr + '.';

  if fsUnderline in f.Style then
    rtnStr := rtnStr + 'U'
  else
    rtnStr := rtnStr + '.';


  Result := rtnStr;
end;

function stickyNote.StringToFont(s: string): TFont;
{  Produces a font from a given string representation [produced by FonttoString.
   The string is read a bit at at time [up to the next :] and this is converted
   to the given font attribute.

   NB :: pos starts from position 1, where copy start at position 1.
         TFontStyles is a set and scanned differently.

   Does not yet handle errors - will there be any ;o)                                              }
var
  p   : integer;
  err : integer;
  chrs: integer;
  clr : TColor;
  Hght: integer;
  nme : string;
  Ortn: integer;
  sze : integer;
  ptch: integer;
  qlty: integer;

  fnt    : TFont;
  fstyles: TFontStyles;
begin
  fnt     := TFont.Create;
  fstyles := [];             //  empty set ??

  p := Pos(':', s);                                 //  Character set of font
  val(copy(s, 0, p - 1), chrs, err);
  Delete(s, 1, p);

  p   := Pos(':', s);                               //  colour of font
  clr := StringToColor(copy(s, 0, p - 1));
  Delete(s, 1, p);

  p := Pos(':', s);                                 //  height of font
  val(copy(s, 0, p - 1), Hght, err);
  Delete(s, 1, p);

  p   := Pos(':', s);                               //  name of font
  nme := copy(s, 0, p - 1);
  Delete(s, 1, p);

  p := Pos(':', s);                                 //  orientation of font
  val(copy(s, 0, p - 1), Ortn, err);
  Delete(s, 1, p);

  p := Pos(':', s);                                 //  size of font
  val(copy(s, 0, p - 1), sze, err);
  Delete(s, 1, p);

  p := Pos(':', s);                                 //  pitch of font
  val(copy(s, 0, p - 1), ptch, err);
  Delete(s, 1, p);

  p := Pos(':', s);                                 //  quality of font
  val(copy(s, 0, p - 1), qlty, err);
  Delete(s, 1, p);

  if Pos('B', s) <> 0 then include(fstyles, fsBold);
  if Pos('I', s) <> 0 then include(fstyles, fsItalic);
  if Pos('S', s) <> 0 then include(fstyles, fsStrikeOut);
  if Pos('U', s) <> 0 then include(fstyles, fsUnderline);

  fnt.Charset     := chrs;                 //  Character set of font
  fnt.Color       := clr;                  //  colour of font
  fnt.Height      := Hght;                 //  height of font
  fnt.Name        := nme;                  //  name of font
  fnt.Orientation := Ortn;                 //  orientation of font
  fnt.Size        := sze;                  //  size of font
  fnt.Pitch       := TFontPitch(ptch);     //  pitch of font
  fnt.Quality     := TFontQuality(qlty);   //  quality of font
  fnt.Style       := fstyles;

  Result := fnt;
end;
end.


