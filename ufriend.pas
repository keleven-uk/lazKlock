unit uFriend;
{   A Friend class
    Holds the data for a Friend Entry.
    This is adapted from the StickyNote class and used in a similar way..

    This class is manipulated by the Friends class.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  Friend = class

  private
    _fName: string;             //  First name.
    _mName: string;             //  Middle name
    _sName: String;             //  Last Name.
    _id   : integer;            //  unique id of the Friend.

    _email1  : string;          //  email.
    _email2  : string;
    _email3  : string;
    _telNo1  : string;          // telephone numer.
    _telNo2  : string;
    _telNo3  : string;
    _houseNo : string;          //  address
    _address1: string;
    _address2: string;
    _city    : string;
    _postCode: string;
    _county  : string;
    _country : string;
    _webPage : string;          //  Personal or company webpage.
    _dob     : string;          //  Date of Birth.
    _notes   : string;          //  notes.

    procedure error(Const msg : string);

  public
    property fName: string  read _fName write _fName;
    property mName: string  read _mName write _mName;
    property sName: string  read _sName write _sName;
    property id   : integer read _id    write _id;

    property email1  : string  read _email1   write _email1;
    property email2  : string  read _email2   write _email2;
    property email3  : string  read _email3   write _email3;
    property telNo1  : string  read _telNo1   write _telNo1;
    property telNo2  : string  read _telNo2   write _telNo2;
    property telNo3  : string  read _telNo3   write _telNo3;
    property houseNo : string  read _houseNo  write _houseNo;
    property address1: string  read _address1 write _address1;
    property address2: string  read _address2 write _address2;
    property city    : string  read _city     write _city;
    property postCode: string  read _postCode write _postCode;
    property county  : string  read _county   write _county;
    property country : string  read _country  write _country;
    property webPage : string  read _webPage  write _webPage;
    property dob     : string  read _dob      write _dob;
    property notes   : string  read _notes    write _notes;

    constructor Create(sn_id: integer); overload;
    constructor Create(fsOut: TFileStream); overload;
    procedure saveToFile(fsOut: TFileStream);
    procedure copy(f: Friend);
  end;

implementation

constructor Friend.Create(sn_id: integer);
{  Creates a new Friend.    }
begin
  id := sn_id;

  fName := '';
  mName := '';
  sName := '';

  email1   := '';
  email2   := '';
  email3   := '';
  telNo1   := '';
  telNo2   := '';
  telNo3   := '';
  houseNo  := '';
  address1 := '';
  address2 := '';
  city     := '';
  postCode := '';
  county   := '';
  country  := '';
  webPage  := '';
  dob      := '';
  notes    := '';
end;

constructor Friend.Create(fsOut: TFileStream);
{  Creates a new Friend.
   A filestream has to be specified, the Friend is then created
   from data read from the filestream.

   If the filestream is empty a blank Friend is returned.
   An exception is raised if there is an error on write.
}
var
  Len1: Cardinal = 0;
  iID : integer = 0;

  sfName: string   = '';
  smName: string   = '';
  ssName: string   = '';

  semail1  : string = '';
  semail2  : string = '';
  semail3  : string = '';
  stelNo1  : string = '';
  stelNo2  : string = '';
  stelNo3  : string = '';
  shouseNo : string = '';
  saddress1: string = '';
  saddress2: string = '';
  scity    : string = '';
  spostCode: string = '';
  scounty  : string = '';
  scountry : string = '';
  swebPage : string = '';
  sdob     : string = '';
  snotes   : string = '';
begin

  try
    //  need the dummy variables, had the error Can't take the address of constant expressions..
    fsOut.ReadBuffer(iID, sizeof(iID));     //  read id.

    fsout.ReadBuffer(Len1, Sizeof(Len1));   //  read first name.
    SetLength(sfName, Len1);
    fsOut.ReadBuffer(sfName[1], len1);

    fsout.ReadBuffer(Len1, Sizeof(Len1));   //  read middle name.
    SetLength(smName, Len1);
    fsOut.ReadBuffer(smName[1], len1);

    fsout.ReadBuffer(Len1, Sizeof(Len1));   //  read last name.
    SetLength(ssName, Len1);
    fsOut.ReadBuffer(ssName[1], len1);

    fsout.ReadBuffer(Len1, Sizeof(Len1));   //  read email1.
    SetLength(semail1, Len1);
    fsOut.ReadBuffer(semail1[1], len1);

    fsout.ReadBuffer(Len1, Sizeof(Len1));   //  read email2.
    SetLength(semail2, Len1);
    fsOut.ReadBuffer(semail2[1], len1);

    fsout.ReadBuffer(Len1, Sizeof(Len1));   //  read email3.
    SetLength(semail3, Len1);
    fsOut.ReadBuffer(semail3[1], len1);

    fsout.ReadBuffer(Len1, Sizeof(Len1));   //  read telNo1.
    SetLength(stelNo1, Len1);
    fsOut.ReadBuffer(stelNo1[1], len1);

    fsout.ReadBuffer(Len1, Sizeof(Len1));   //  read telNo2.
    SetLength(stelNo2, Len1);
    fsOut.ReadBuffer(stelNo2[1], len1);

    fsout.ReadBuffer(Len1, Sizeof(Len1));   //  read telNo3.
    SetLength(stelNo3, Len1);
    fsOut.ReadBuffer(stelNo3[1], len1);

    fsout.ReadBuffer(Len1, Sizeof(Len1));   //  read houseNo.
    SetLength(shouseNo, Len1);
    fsOut.ReadBuffer(shouseNo[1], len1);

    fsout.ReadBuffer(Len1, Sizeof(Len1));   //  read address1.
    SetLength(saddress1, Len1);
    fsOut.ReadBuffer(saddress1[1], len1);

    fsout.ReadBuffer(Len1, Sizeof(Len1));   //  read address2.
    SetLength(saddress2, Len1);
    fsOut.ReadBuffer(saddress2[1], len1);

    fsout.ReadBuffer(Len1, Sizeof(Len1));   //  read city.
    SetLength(scity, Len1);
    fsOut.ReadBuffer(scity[1], len1);

    fsout.ReadBuffer(Len1, Sizeof(Len1));   //  post Code.
    SetLength(spostCode, Len1);
    fsOut.ReadBuffer(spostCode[1], len1);

    fsout.ReadBuffer(Len1, Sizeof(Len1));   //  county.
    SetLength(scounty, Len1);
    fsOut.ReadBuffer(scounty[1], len1);

    fsout.ReadBuffer(Len1, Sizeof(Len1));   //  country.
    SetLength(scountry, Len1);
    fsOut.ReadBuffer(scountry[1], len1);

    fsout.ReadBuffer(Len1, Sizeof(Len1));   //  webPage.
    SetLength(swebPage, Len1);
    fsOut.ReadBuffer(swebPage[1], len1);

    fsout.ReadBuffer(Len1, Sizeof(Len1));   //  dob.
    SetLength(sdob, Len1);
    fsOut.ReadBuffer(sdob[1], len1);

    fsout.ReadBuffer(Len1, Sizeof(Len1));   //  notes.
    SetLength(snotes, Len1);
    fsOut.ReadBuffer(snotes[1], len1);

    id   := iID;

    fName := sfName;
    mName := smName;
    sName := ssName;

    email1   := semail1;
    email2   := semail2;
    email3   := semail3;
    telNo1   := stelNo1;
    telNo2   := stelNo2;
    telNo3   := stelNo3;
    houseNo  := shouseNo;
    address1 := saddress1;
    address2 := saddress2;
    city     := scity;
    postCode := spostCode;
    county   := scounty;
    country  := scountry;
    webPage  := swebPage;
    dob      := sdob ;
    notes    := snotes;

  except
    on E: Exception do
      error('Error on Events Read' + LineEnding + E.Message);
  end;
end;

procedure Friend.saveToFile(fsOut: TFileStream);
{  Save a Friend to the specified filestream.
   An exception is raised if there is an error on write.
}
var
  Len1: Cardinal;
begin
  try
    fsOut.WriteBuffer(id, sizeof(id));                 //  write id.

    Len1 := Length(fName);                             //  write first name.
    fsOut.WriteBuffer(Len1, SizeOf(Len1));
    fsOut.WriteBuffer(fName[1], Len1);

    Len1 := Length(mName);                             //  write middle name.
    fsOut.WriteBuffer(Len1, SizeOf(Len1));
    fsOut.WriteBuffer(mName[1], Len1);

    Len1 := Length(sName);                             //  write [last] surname.
    fsOut.WriteBuffer(Len1, SizeOf(Len1));
    fsOut.WriteBuffer(sName[1], Len1);

    Len1 := Length(email1);                            //  write email1.
    fsOut.WriteBuffer(Len1, SizeOf(Len1));
    fsOut.WriteBuffer(email1[1], Len1);

    Len1 := Length(email2);                            //  write email2.
    fsOut.WriteBuffer(Len1, SizeOf(Len1));
    fsOut.WriteBuffer(email2[1], Len1);

    Len1 := Length(email3);                            //  write email3.
    fsOut.WriteBuffer(Len1, SizeOf(Len1));
    fsOut.WriteBuffer(email3[1], Len1);

    Len1 := Length(telNo1);                            //  write telNo1.
    fsOut.WriteBuffer(Len1, SizeOf(Len1));
    fsOut.WriteBuffer(telNo1[1], Len1);

    Len1 := Length(telNo2);                            //  write telNo2.
    fsOut.WriteBuffer(Len1, SizeOf(Len1));
    fsOut.WriteBuffer(telNo2[1], Len1);

    Len1 := Length(telNo3);                            //  write telNo3.
    fsOut.WriteBuffer(Len1, SizeOf(Len1));
    fsOut.WriteBuffer(telNo3[1], Len1);

    Len1 := Length(houseNo);                           //  write houseNo.
    fsOut.WriteBuffer(Len1, SizeOf(Len1));
    fsOut.WriteBuffer(houseNo[1], Len1);

    Len1 := Length(address1);                          //  write address1.
    fsOut.WriteBuffer(Len1, SizeOf(Len1));
    fsOut.WriteBuffer(address1[1], Len1);

    Len1 := Length(address2);                          //  write address2.
    fsOut.WriteBuffer(Len1, SizeOf(Len1));
    fsOut.WriteBuffer(address2[1], Len1);

    Len1 := Length(city);                              //  write city.
    fsOut.WriteBuffer(Len1, SizeOf(Len1));
    fsOut.WriteBuffer(city[1], Len1);

    Len1 := Length(postCode);                          //  write postCode.
    fsOut.WriteBuffer(Len1, SizeOf(Len1));
    fsOut.WriteBuffer(postCode[1], Len1);

    Len1 := Length(county);                            //  write county.
    fsOut.WriteBuffer(Len1, SizeOf(Len1));
    fsOut.WriteBuffer(county[1], Len1);

    Len1 := Length(country);                           //  write country.
    fsOut.WriteBuffer(Len1, SizeOf(Len1));
    fsOut.WriteBuffer(country[1], Len1);

    Len1 := Length(notes);                             //  write notes.
    fsOut.WriteBuffer(Len1, SizeOf(Len1));
    fsOut.WriteBuffer(notes[1], Len1);

  except
    on E: Exception do
      error('Error on Events Write' + LineEnding + E.Message);
  end;
End;

procedure Friend.copy(f: Friend);
{  Copies the supplies Friend into the current Friend.    }
begin
  id := f.id;

  fName := f.fName;
  mName := f.mName;
  sName := f.sName;

  email1  := f.email1;
  email2  := f.email2;
  email3  := f.email3;
  telNo1  := f.telNo1;
  telNo2  := f.telNo2;
  telNo3  := f.telNo3;
  houseNo := f.houseNo;
  address1:= f.address1;
  address2:= f.address2;
  city    := f.city;
  postCode:= f.postCode;
  county  := f.county;
  country := f.dob;
  notes   := f.notes;
end;

procedure Friend.error(Const msg : string);
{  Raises a custom exception.    }
begin
  raise exception.create(Msg) at
    get_caller_addr(get_frame),
    get_caller_frame(get_frame);
end;
end.

