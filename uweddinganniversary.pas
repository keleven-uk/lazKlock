unit uweddingAnniversary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  WeddingGifts = class

  private
    procedure loadGifts;
  public
    constructor Create; overload;
    destructor Destroy; override;

    function whichGift(year: integer): string;
  end;


implementation

VAR
  gnme: string;
  gift: TStringList;

constructor WeddingGifts.Create; overload;
{  set up some variables on create.    }
begin
  gift := TStringList.Create;

  gnme := 'WeddingGifts.txt';
  loadGifts;
end;

destructor WeddingGifts.Destroy;
{  run on destroy.    }
begin
  gift.free;
  inherited;
end;

function WeddingGifts.whichGift(year: integer): string;
begin
  //if gift.Count = 1 then
  //  return gift.Strings[0];
  //else
  //  return gift.Strings[year];

end;

procedure WeddingGifts.loadGifts;
{  Populates Gifts with a list of wedding gifts.
   The gifts are [should] be held in a text file in the application folder.
   The appropriate gift for a year should be at that index.

   If the file is not found, load index 0 with happy.
}
begin
 try
    gift.LoadFromFile(gnme);
  except
    on Exception do
    begin
      gift.Add('Happy');
    end;
  end;
end;

end.

