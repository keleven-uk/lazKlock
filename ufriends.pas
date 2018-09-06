unit uFriends;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uFriend, fgl;

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
    procedure amend();
    function retrieve(id: integer): Friend;
    procedure Remove(pos: integer);
    procedure saveFriends;
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
  friendsFile         := GetAppConfigDir(False) + 'Friend.bin';
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

  //saveFriends;
end;

procedure Friends.amend();
{  Amends a friend .
   The friend store is then saved to file.
}
begin

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

end.

