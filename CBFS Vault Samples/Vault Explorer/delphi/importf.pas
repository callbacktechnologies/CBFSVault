unit importf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, 
  StrRes, ExtCtrls, ComCtrls;

type
  TFormImport = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    edDiskPath: TEdit;
    lbDiskPath: TLabel;
    lbMask: TLabel;
    edMask: TEdit;
    cbRecursive: TCheckBox;
    btnBrowse: TButton;
    cbEncryption: TCheckBox;
    edPassword: TEdit;
    lbPassword: TLabel;
    lbConfirmation: TLabel;
    edConfirmation: TEdit;
    cbCompress: TCheckBox;
    lbCompressionLevel: TLabel;
    lbCompressionLevelValue: TLabel;
    tbCompressionLevel: TTrackBar;
    procedure btnBrowseClick(Sender: TObject);
    procedure cbEncryptionClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure tbCompressionLevelChange(Sender: TObject);
    procedure cbCompressClick(Sender: TObject);
    procedure edDiskPathChange(Sender: TObject);
  private
  public
    EncryptionChecked: Boolean;
  end;

var
FormImport :TFormImport;

implementation

uses
  vaultexplorerf;

{$R *.DFM}

procedure TFormImport.btnBrowseClick(Sender: TObject);
var
  Directory: String;
begin
  Directory := edDiskPath.Text;
  if Directory = '' then
    Directory := GetCurrentDir;
  if ChooseFolder(Self, Directory, '') then
  begin
    edDiskPath.Text := Directory;
    btnOK.Enabled := edDiskPath.Text <> '';    
  end;
end;

procedure TFormImport.cbEncryptionClick(Sender: TObject);
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

procedure TFormImport.btnOKClick(Sender: TObject);
begin
  if edPassword.Text <> edConfirmation.Text then
  begin
    MessageDlg(PasswordsNotEqual, mtError, [mbOK], 0);
    Exit;
  end;

  ModalResult := mrOK;
end;

procedure TFormImport.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFormImport.tbCompressionLevelChange(Sender: TObject);
var
  CompressionLevel: Integer;
begin
  CompressionLevel := tbCompressionLevel.Position;
  if CompressionLevel = 0 then
    lbCompressionLevelValue.Caption := 'Default'
  else
  if CompressionLevel = 1 then
    lbCompressionLevelValue.Caption := 'Min'
  else
  if CompressionLevel = 9 then
    lbCompressionLevelValue.Caption := 'Max'
  else
    lbCompressionLevelValue.Caption := IntToStr(CompressionLevel);
end;

procedure TFormImport.cbCompressClick(Sender: TObject);
begin
  tbCompressionLevel.Enabled := cbCompress.Checked;
  lbCompressionLevel.Enabled := cbCompress.Checked;
  lbCompressionLevelValue.Enabled := cbCompress.Checked;
end;

procedure TFormImport.edDiskPathChange(Sender: TObject);
begin
  btnOK.Enabled := edDiskPath.Text <> '';
end;

end.
