unit uFriends;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uFriend, fgl, Dialogs, FileUtil;

type
  Friends = class

  private
    _friendsFile : string;
    _friendsCount: integer;

    property friendsFile: string read _friendsFile write _friendsFile;
  public
    property friendsCount: integer read _friendsCount write _friendsCount;

    constructor Create; overload;
    destructor Destroy; override;
    procedure New(f: Friend);
    procedure amend(pos: integer; f : Friend);
    function retrieve(id: integer): Friend;
    procedure Remove(pos: integer);
    procedure saveFriends;
    procedure restoreFriends;
  end;

  //  effectively a dictionary.
  keyStore = specialize TFPGMap<integer, Friend>;

VAR
  friendsStore: keyStore;

implementation

uses
  formklock;


constructor Friends.Create; overload;
{  set up some variables on create.    }
begin
  friendsFile         := GetAppConfigDir(False) + 'Friends.bin';
  friendsStore        := keyStore.Create;       //  initial eventStore
  friendsStore.Sorted := true;
  friendsCount        := 0;

end;

destructor Friends.Destroy;
{  run on destroy.    }
begin
  friendsStore.free;

  inherited;
end;

procedure Friends.New(f: Friend);
{  Creates a new Friend.  This is called from the host program.
   A new Friend is created and added to the store.
   The Friends store is then saved to file.
}
VAR
  ff: Friend;                   //  Friend.
begin
  ff := Friend.Create(0);
  ff.copy(f);

  friendsStore.Add(friendsCount, ff);

  friendsCount := friendsCount + 1;

  saveFriends;
end;

procedure Friends.amend(pos: integer; f : Friend);
{  Amends a friend .
   The friend store is then saved to file.
}
begin
  friendsStore.Data[pos].copy(f);
  saveFriends;
end;

function Friends.retrieve(id: integer): Friend;
{returns a Friend out of the store at position id.    }
VAR
  f: Friend;                   //  Events.
begin
  f := Friend.Create(0);
  f.copy(friendsStore.Data[id]);

  result := f
end;

procedure Friends.Remove(pos: integer);
{  Remove a Friend from the store at a given position.    }
begin
  friendsStore.Remove(pos);
  friendsCount := friendsCount - 1;
  saveFriends;
end;

procedure Friends.saveFriends;
{  Write out the contents of the Friends Store to a binary file.    }
var
  fileOut: TFileStream;
  f      : integer;
begin
  if friendsCount = 0 then
  begin
    DeleteFile(friendsFile);    //  friends file is empty, delete if there.
    exit;
  end;

  fileOut := TFileStream.Create(friendsFile, fmCreate or fmShareDenyWrite);

  try
    for f := 0 to friendsCount - 1 do
    begin
      try
      friendsStore.Data[f].saveToFile(fileOut);
      except
        on E: EInOutError do
      end;
    end;
  finally
    fileOut.Free;
  end;
end;

procedure Friends.restoreFriends;
{  Read in a binary file and convert contents to friends and populate the store.
}
var
  fileIn: TFileStream;
begin
  friendsCount := 0;

  if not fileExists(friendsFile) then exit;       //  file not there then exit
  if fileSize(friendsFile) = 0   then exit;       //  empty file then exit.

  fileIn := TFileStream.Create(friendsFile, fmOpenRead or fmShareDenyWrite);

  try
    repeat
      try
        friendsStore.Add(friendsCount, Friend.Create(fileIn));
        friendsCount := friendsCount + 1;
        except
          on E: EInOutError do
          ShowMessage('ERROR : Reading Friends file');
      end;  //  try
      until FileIn.Position >= fileIn.Size;
   finally
    fileIn.Free;
  end;        //  try
end;


end.

