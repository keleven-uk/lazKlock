unit formFriendsInput;

{ Allows a friend to be input into Klock.    }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, EditBtn, uFriend;

type

  { TfrmFriendsInput }

  TfrmFriendsInput = class(TForm)
    btnCancel         : TButton;
    btnOK             : TButton;
    btnClose          : TButton;
    btnDelete         : TButton;
    chckBxAddtoEvents : TCheckBox;
    dtEdtDOB          : TDateEdit;
    edtHomePage       : TEdit;
    edtCounty         : TEdit;
    edtCountry        : TEdit;
    edtPostCode       : TEdit;
    edtCity           : TEdit;
    edtAddress2       : TEdit;
    edtAddress1       : TEdit;
    edtHouseNumber    : TEdit;
    edtTelNo1         : TEdit;
    edtTelNo2         : TEdit;
    edtTelNo3         : TEdit;
    edtFirstName      : TEdit;
    edtEmail1         : TEdit;
    edtLastName       : TEdit;
    edtEmail3         : TEdit;
    edtMiddleName     : TEdit;
    edtEmail2         : TEdit;
    GroupBox1         : TGroupBox;
    Label1            : TLabel;
    Label10           : TLabel;
    Label11           : TLabel;
    Label12           : TLabel;
    Label13           : TLabel;
    Label14           : TLabel;
    Label15           : TLabel;
    Label16           : TLabel;
    Label2            : TLabel;
    Label3            : TLabel;
    Label4            : TLabel;
    Label5            : TLabel;
    Label6            : TLabel;
    Label7            : TLabel;
    Label8            : TLabel;
    Label9            : TLabel;
    moNotes           : TMemo;
    Panel1            : TPanel;

    procedure btnCancelClick(Sender: TObject);
    procedure btnCloseClick (Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnOKClick    (Sender: TObject);
    procedure FormClose     (Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate    (Sender: TObject);
    procedure FormShow      (Sender: TObject);
  private
    function isBlank     (txt: string):string;
    procedure setReadOnly(readOnly: boolean);
    procedure displayFriend;
  public

  end;

var
  frmFriendsInput: TfrmFriendsInput;
  ReallyCanClose : Boolean;
  Mode           : string;          //  mode of the form - either New, View
  pos            : integer;         //  position of the friend in the store.

implementation

uses
  formklock;

{$R *.lfm}

{ TfrmFriendsInput }

procedure TfrmFriendsInput.FormCreate(Sender: TObject);
begin
  kLog.writeLog('formFriendsInput Create');
end;

procedure TfrmFriendsInput.FormShow(Sender: TObject);
{  On form show, customise the form as desired.    }
begin
  kLog.writeLog('formFriendsInput Show in mode ' + mode);

  case mode of
    'NEW':                                     //  New Friend.
      begin
        btnOK.Visible     := true;
        btnCancel.Visible := true;
        btnClose.Visible  := false;
        btnDelete.Visible := false;
        setReadOnly(False);
      end;
    'VIEW':
      begin
        btnOK.Visible     := false;
        btnCancel.Visible := false;
        btnClose.Visible  := true;
        btnDelete.Visible := false;
        ReallyCanClose    := true;             //  don't really care, view mode, allow just to close.
        setReadOnly(true);
        displayFriend;
      end;
    'EDIT':                                    //  Edit a friend.
      begin
        btnOK.Visible     := true;
        btnCancel.Visible := true;
        btnClose.Visible  := false;
        btnDelete.Visible := false;
        setReadOnly(false);
        displayFriend;
      end;
    'DELETE':                                  // Delete a friend.
      begin
        btnOK.Visible     := false;
        btnCancel.Visible := true;
        btnClose.Visible  := false;
        btnDelete.Visible := true;
        setReadOnly(true);
        displayFriend;
      end;
  end;
end;

procedure TfrmFriendsInput.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if not ReallyCanClose then
    CloseAction := caNone
  else
    begin
      kLog.writeLog('formFriendsInput Close');
      CloseAction := caFree;
    end;

end;
//
// ********************************************************* Buttons ******************
//
procedure TfrmFriendsInput.btnOKClick(Sender: TObject);
{  If ok is clicked, add the friends to the store.

   This routine is called by both edit and new.
}
VAR
  f: friend;
begin
  kLog.writeLog('formFriendsInput OK Clicked ');

  if (edtFirstName.Text = '') or (edtLastName.Text = '') then
  begin
    showmessage('A valid First name and last name must be entered');
    ReallyCanClose := false;
    exit;
  end;

  ReallyCanClose := true;

  kLog.writeLog(format('[TfrmFriendsInput.OKButtonClick] Adding friend %d', [fr.friendsCount]));
  f := Friend.Create(fr.friendsCount);

  f.fName    := edtFirstName.Text;
  f.mName    := isBlank(edtMiddleName.Text);
  f.sName    := edtLastName.Text;
  f.email1   := isBlank(edtEmail1.Text);
  f.email2   := isBlank(edtEmail2.Text);
  f.email3   := isBlank(edtEmail3.Text);
  f.telNo1   := isBlank(edtTelNo1.Text);
  f.telNo2   := isBlank(edtTelNo2.Text);
  f.telNo3   := isBlank(edtTelNo3.Text);
  f.houseNo  := isBlank(edtHouseNumber.Text);
  f.address1 := isBlank(edtAddress1.Text);
  f.address2 := isBlank(edtAddress2.Text);
  f.city     := isBlank(edtCity.Text);
  f.postCode := isBlank(edtPostCode.Text);
  f.county   := isBlank(edtCounty.Text);
  f.country  := isBlank(edtCountry.Text);
  f.webPage  := isBlank(edtHomePage.Text);
  f.dob      := isBlank(DateTimeToStr(dtEdtDOB.Date));
  f.notes    := isBlank(moNotes.Text);

  if mode = 'NEW' then
    fr.New(f)
  else if mode = 'EDIT' then
    fr.amend(pos, f);

  f.Free;

  close;
end;

procedure TfrmFriendsInput.btnCancelClick(Sender: TObject);
begin
  kLog.writeLog('formFriendsInput Cancel Clicked ');
  ReallyCanClose := true;
  Close;
end;

procedure TfrmFriendsInput.btnCloseClick(Sender: TObject);
begin
  kLog.writeLog('formFriendsInput Close Clicked ');
  close;
end;

procedure TfrmFriendsInput.btnDeleteClick(Sender: TObject);
begin
  kLog.writeLog('formFriendsInput Delete Clicked ');
  if QuestionDlg ('Event Delete',
                  'Do You Really Want To Delete This Event',
                   mtCustom, [mrYes,'yes', mrNo, 'No', 'IsDefault'],'')  = mrYes then
  begin
    fr.Remove(pos);
  end;

  close;
end;

function TfrmFriendsInput.isBlank(txt: string):string;
{  Either return the text, or if the text is blank, return a space.

   There seems to be a problem with writing blank strings to files later
   on in the program.
}
begin

  if txt = '' then
    Result := ' '
  else
    Result := txt;
end;
//
// ********************************************************* private stuff ******************
//
procedure TfrmFriendsInput.setReadOnly(readOnly: boolean);
{    Sets all the fields readonly status to mode - either true or false.}
begin
  edtFirstName.ReadOnly   := readOnly;
  edtFirstName.Enabled    := not(readOnly);
  edtMiddleName.ReadOnly  := readOnly;
  edtMiddleName.Enabled   := not(readOnly);
  edtLastName.ReadOnly    := readOnly;
  edtLastName.Enabled     := not(readOnly);
  edtEmail1.ReadOnly      := readOnly;
  edtEmail1.Enabled       := not(readOnly);
  edtEmail2.ReadOnly      := readOnly;
  edtEmail2.Enabled       := not(readOnly);
  edtEmail3.ReadOnly      := readOnly;
  edtEmail3.Enabled       := not(readOnly);
  edtTelNo1.ReadOnly      := readOnly;
  edtTelNo1.Enabled       := not(readOnly);
  edtTelNo2.ReadOnly      := readOnly;
  edtTelNo2.Enabled       := not(readOnly);
  edtTelNo3.ReadOnly      := readOnly;
  edtTelNo3.Enabled       := not(readOnly);
  edtHouseNumber.ReadOnly := readOnly;
  edtHouseNumber.Enabled  := not(readOnly);
  edtAddress1.ReadOnly    := readOnly;
  edtAddress1.Enabled     := not(readOnly);
  edtAddress2.ReadOnly    := readOnly;
  edtAddress2.Enabled     := not(readOnly);
  edtCity.ReadOnly        := readOnly;
  edtCity.Enabled         := not(readOnly);
  edtPostCode.ReadOnly    := readOnly;
  edtPostCode.Enabled     := not(readOnly);
  edtCounty.ReadOnly      := readOnly;
  edtCounty.Enabled       := not(readOnly);
  edtCountry.ReadOnly     := readOnly;
  edtCountry.Enabled      := not(readOnly);
  edtHomePage.ReadOnly    := readOnly;
  edtHomePage.Enabled     := not(readOnly);
  dtEdtDOB.ReadOnly       := readOnly;
  dtEdtDOB.Enabled        := not(readOnly);
  moNotes.ReadOnly        := readOnly;
  moNotes.Enabled         := not(readOnly);
  moNotes.ParentColor     := readOnly;
end;

procedure TfrmFriendsInput.displayFriend;
{    displays a friends, it sets all the relevent fields on the form
     to the relevent fields of a friends object.
}
VAR
  f: friend;                    //  Friend.
begin
  f := Friend.Create(0);
  f := fr.retrieve(pos);

  edtFirstName.Text   := f.fName;
  edtMiddleName.Text  := f.mName;
  edtLastName.Text    := f.sName;
  edtEmail1.Text      := f.email1;
  edtEmail2.Text      := f.email2;
  edtEmail3.Text      := f.email3;
  edtTelNo1.Text      := f.telNo1;
  edtTelNo2.Text      := f.telNo2;
  edtTelNo3.Text      := f.telNo3;
  edtHouseNumber.Text := f.houseNo;
  edtAddress1.Text    := f.address1;
  edtAddress2.Text    := f.address2;
  edtCity.Text        := f.city;
  edtPostCode.Text    := f.postCode;
  edtCounty.Text      := f.county;
  edtCountry.Text     := f.country;
  edtHomePage.Text    := f.webPage;
  dtEdtDOB.Date       := strToDate(f.dob);
  moNotes.Text        := f.notes;

  f.Free;
end;

end.

