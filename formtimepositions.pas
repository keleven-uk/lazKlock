unit formTimePositions;

{  Allows the user to change the screen position of the extra Klocks.
   These can sometimes disappear - usually if run on a different monitor configuration.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin;

type

  { TfrmTimePositions }

  TfrmTimePositions = class(TForm)
    btnOK                      : TButton;
    btnCancel                  : TButton;
    lblLeft: TLabel;
    lblTop: TLabel;
    lblAnalogueKlock           : TLabel;
    lblLEDKlock                : TLabel;
    lblBinaryKlock             : TLabel;
    lblSmallTextKlock          : TLabel;
    lblFloatingTextKlock       : TLabel;
    Panel1                     : TPanel;
    Panel2                     : TPanel;
    spnEdtAnalogueKlockLeft    : TSpinEdit;
    spnEdtFloatingTextKlockTop : TSpinEdit;
    spnEdtAnalogueKlockTop     : TSpinEdit;
    spnEdtLEDKlockLeft         : TSpinEdit;
    spnEdtLEDKlockTop          : TSpinEdit;
    spnEdtBinaryKlockLeft      : TSpinEdit;
    spnEdtBinaryKlockTop       : TSpinEdit;
    spnEdtSmallTextKlockLeft   : TSpinEdit;
    spnEdtSmallTextKlockTop    : TSpinEdit;
    spnEdtFloatingTextKlockLeft: TSpinEdit;

    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  frmTimePositions: TfrmTimePositions;

implementation

uses
  formklock;

{$R *.lfm}

{ TfrmTimePositions }

procedure TfrmTimePositions.FormCreate(Sender: TObject);
{  On form create, copy user options for time positions into spin edits.    }
begin
  klog.writeLog('frmTimePositions Create');

  spnEdtAnalogueKlockLeft.Value     := userOptions.analogueFormLeft;
  spnEdtAnalogueKlockTop.Value      := userOptions.analogueFormTop;
  spnEdtLEDKlockLeft.Value          := userOptions.LEDFormLeft;
  spnEdtLEDKlockTop.Value           := userOptions.LEDFormTop;
  spnEdtBinaryKlockLeft.Value       := userOptions.BinaryFormLeft;
  spnEdtBinaryKlockTop.Value        := userOptions.BinaryFormTop;
  spnEdtSmallTextKlockLeft.Value    := userOptions.smallTextFormLeft;
  spnEdtSmallTextKlockTop.Value     := userOptions.smallTextFormTop;
  spnEdtFloatingTextKlockLeft.Value := userOptions.floatingTextFormLeft;
  spnEdtFloatingTextKlockTop.Value  := userOptions.floatingTextFormTop;
end;

procedure TfrmTimePositions.btnOKClick(Sender: TObject);
{  OK has been clicked, update user options with new positions.
   Does not check is been changed, just saves.
}
begin
  userOptions.analogueFormLeft     := spnEdtAnalogueKlockLeft.Value;
  userOptions.analogueFormTop      := spnEdtAnalogueKlockTop.Value;
  userOptions.LEDFormLeft          := spnEdtLEDKlockLeft.Value;
  userOptions.LEDFormTop           := spnEdtLEDKlockTop.Value;
  userOptions.BinaryFormLeft       := spnEdtBinaryKlockLeft.Value;
  userOptions.BinaryFormTop        := spnEdtBinaryKlockTop.Value;
  userOptions.smallTextFormLeft    := spnEdtSmallTextKlockLeft.Value;
  userOptions.smallTextFormTop     := spnEdtSmallTextKlockTop.Value;
  userOptions.floatingTextFormLeft := spnEdtFloatingTextKlockLeft.Value;
  userOptions.floatingTextFormTop  := spnEdtFloatingTextKlockTop.Value;

  userOptions.writeCurrentOptions;
  close;
end;

procedure TfrmTimePositions.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TfrmTimePositions.btnCancelClick(Sender: TObject);
{  Cancel has been clicked, close and save nowt.    }
begin
  close;
end;

end.

