unit formSplashScreen;

{  Displays a splash screen when Klock starts.    }
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TfrmSplashScreen }

  TfrmSplashScreen = class(TForm)
    Image1              : TImage;
    MemoSplashScreenInfo: TMemo;
    MemoSplashScreenData: TMemo;

    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure Image2Click(Sender: TObject);

  private

  end;

var
  frmSplashScreen: TfrmSplashScreen;

implementation

uses
  formklock;


{$R *.lfm}

{ TfrmSplashScreen }


procedure TfrmSplashScreen.FormClose(Sender: TObject;  var CloseAction: TCloseAction);
begin
  CloseAction     := caFree;
  frmSplashScreen := nil;
  CloseAction     := caFree;
end;

procedure TfrmSplashScreen.Image2Click(Sender: TObject);
begin
  close;
  frmmain.Close;
end;

end.

