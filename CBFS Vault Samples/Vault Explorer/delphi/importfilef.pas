unit importfilef;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, 
  StrRes, ExtCtrls, ComCtrls;

type
  TFormImportfile = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    btnAll: TButton;
    lbText: TLabel;
    cbEncryption: TCheckBox;
    edPassword: TEdit;
    lbPassword: TLabel;
    lbConfirmation: TLabel;
    edConfirmation: TEdit;
    cbCompress: TCheckBox;
    lbCompressionLevel: TLabel;
    lbCompressionLevelValue: TLabel;
    tbCompressionLevel: TTrackBar;
    procedure cbEncryptionClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnAllClick(Sender: TObject);
    procedure tbCompressionLevelChange(Sender: TObject);
    procedure cbCompressClick(Sender: TObject);
  private
  public
    EncryptionChecked: Boolean;
  end;

var
FormImportfile :TFormImportfile;

implementation

uses
  vaultexplorerf;

{$R *.DFM}

procedure TFormImportfile.cbEncryptionClick(Sender: TObject);
begin
  if cbEncryption.Checked and
     not RegistrationKeyOK and not EncryptionChecked then
  begin
    ShowRegistrationKeyWarning;
    EncryptionChecked := true;
  end;

  lbPassword.Enabled := cbEncryption.Checked;
  edPassword.Enabled := cbEncryption.Checked;
  lbConfirmation.Enabled := cbEncryption.Checked;
  edConfirmation.Enabled := cbEncryption.Checked;
end;

procedure TFormImportfile.btnAllClick(Sender: TObject);
begin
  if edPassword.Text <> edConfirmation.Text then
  begin
    MessageDlg(PasswordsNotEqual, mtError, [mbOK], 0);
    Exit;
  end;

  ModalResult := mrYesToAll;
end;

procedure TFormImportfile.btnOKClick(Sender: TObject);
begin
  if edPassword.Text <> edConfirmation.Text then
  begin
    MessageDlg(PasswordsNotEqual, mtError, [mbOK], 0);
    Exit;
  end;

  ModalResult := mrOK;
end;

procedure TFormImportfile.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFormImportfile.tbCompressionLevelChange(Sender: TObject);
var
  CompressionLevel: Integer;
begin
  CompressionLevel := tbCompressionLevel.Position;
  if CompressionLevel = 0 then
    lbCompressionLevelValue.Caption := 'Default'
  else
    lbCompressionLevelValue.Caption := IntToStr(CompressionLevel);
end;

procedure TFormImportfile.cbCompressClick(Sender: TObject);
begin
  tbCompressionLevel.Enabled := cbCompress.Checked;
  lbCompressionLevel.Enabled := cbCompress.Checked;
  lbCompressionLevelValue.Enabled := cbCompress.Checked;
end;

end.
