unit filepropsf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls,
  importfilef, vaultselectfilef, strres,
  cbvcore, cbvcbvault, cbvconstants;

type
  TFormFileprops = class(TForm)
    pnlButtons: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    PageControl: TPageControl;
    tabGeneral: TTabSheet;
    bvl1: TBevel;
    imgIcon: TImage;
    edtFileName: TEdit;
    bvl2: TBevel;
    lblLocation: TLabel;
    edtLocation: TEdit;
    lblSize: TLabel;
    edtSize: TEdit;
    bvl3: TBevel;
    lblCreated: TLabel;
    edtCreated: TEdit;
    lblModified: TLabel;
    edtModified: TEdit;
    lblAccessed: TLabel;
    edtAccessed: TEdit;
    lblAttributes: TLabel;
    chbDirectory: TCheckBox;
    chbSymLink: TCheckBox;
    chbEncrypted: TCheckBox;
    chbCompressed: TCheckBox;
    chbReadOnly: TCheckBox;
    chbArchive: TCheckBox;
    chbHidden: TCheckBox;
    chbSystem: TCheckBox;
    chbTemporary: TCheckBox;
    btnAdvanced: TButton;
    tabLink: TTabSheet;
    Bevel1: TBevel;
    edFileName2: TEdit;
    imgIcon2: TImage;
    Bevel2: TBevel;
    lbTargetType: TLabel;
    dlbTargetType: TLabel;
    lbTarget: TLabel;
    edTarget: TEdit;
    btnChangeTarget: TButton;
    procedure btnAdvancedClick(Sender: TObject);
    procedure tabLinkShow(Sender: TObject);
    procedure btnChangeTargetClick(Sender: TObject);
    procedure edTargetChange(Sender: TObject);
  private
    FAttributes: LongWord;
    FCompression: Integer;
    FCompressionLevel: LongWord;
    FEncryption: Integer;
    FPassword: WideString;
    FFileName: WideString;
    FVault: TcbvCBVault;

    procedure SetCompression(const Value: Integer);
    procedure SetEncryption(const Value: Integer);
    function GetAttributes: LongWord;
    procedure SetAttributes(const Value: LongWord);
    procedure SetFileName(const Value: WideString);

    procedure UpdateTargetType;
  public
    property FileName: WideString read FFileName write SetFileName;
    property Vault: TcbvCBVault read FVault write FVault;

    property Attributes: LongWord read GetAttributes write SetAttributes;

    property Compression: Integer read FCompression write SetCompression;
    property CompressionLevel: LongWord read FCompressionLevel write FCompressionLevel;
    property Encryption: Integer read FEncryption write SetEncryption;
    property Password: WideString read FPassword write FPassword;
  end;

var
  FormFileprops: TFormFileprops;

implementation

uses
  vaultexplorerf;

{$R *.DFM}

{ TfrmFileProperties }

procedure TFormFileprops.btnAdvancedClick(Sender: TObject);
begin
  with TFormImportfile.Create(Self) do
    try
      Caption := 'Advanced Attributes';
      lbText.Caption := Format(FILE_PROPS_SelectMessage, [string(FileName)]);
      cbCompress.Checked := (Compression <> VAULT_CM_NONE);
      tbCompressionLevel.Position := CompressionLevel;
      EncryptionChecked := (Encryption <> VAULT_EM_NONE);
      cbEncryption.Checked := (Encryption <> VAULT_EM_NONE);
      edPassword.Text := Password;
      edConfirmation.Text := Password;
      if ShowModal = mrOK then
      begin
        if cbEncryption.Checked then
        begin
          Encryption := VAULT_EM_DEFAULT;
          Password := edPassword.Text;
        end
        else
        begin
          Encryption := VAULT_EM_NONE;
          Password := '';
        end;

        if cbCompress.Checked then
        begin
          if tbCompressionLevel.Position = 0 then
          begin
            Compression := VAULT_CM_DEFAULT;
            CompressionLevel := 0;
          end
          else
          begin
            Compression := VAULT_CM_ZLIB;
            CompressionLevel := tbCompressionLevel.Position;
          end;
        end
        else
        begin
          Compression := VAULT_CM_NONE;
          CompressionLevel := 0;
        end;
      end;
    finally
      Free;
    end;
end;

function TFormFileprops.GetAttributes: LongWord;
var
  i: Integer;
  CheckBox: TCheckBox;
begin
  Result := FAttributes and VAULT_FATTR_FILE;
  for i := 0 to tabGeneral.ControlCount - 1 do
    if tabGeneral.Controls[i] is TCheckBox then
    begin
      CheckBox := TCheckBox(tabGeneral.Controls[i]);
      if (CheckBox.Tag <> 0) and CheckBox.Checked then
        Result := Result or LongWord(CheckBox.Tag);
    end;
end;

procedure TFormFileprops.SetAttributes(const Value: LongWord);
var
  i: Integer;
  CheckBox: TCheckBox;
begin
  FAttributes := Value;
  for i := 0 to tabGeneral.ControlCount - 1 do
    if (tabGeneral.Controls[i] is TCheckBox) then
    begin
      CheckBox := TCheckBox(tabGeneral.Controls[i]);
      if CheckBox.Tag <> 0 then
        CheckBox.Checked := (FAttributes and CheckBox.Tag) <> 0;
    end;
end;

procedure TFormFileprops.SetCompression(const Value: Integer);
begin
  FCompression := Value;
  chbCompressed.Checked := (Value <> VAULT_CM_NONE);
end;

procedure TFormFileprops.SetEncryption(const Value: Integer);
begin
  FEncryption := Value;
  chbEncrypted.Checked := (Value <> VAULT_EM_NONE);
end;

procedure TFormFileprops.SetFileName(const Value: WideString);
var
  Search: Int64;
  DestinationName: String;
begin
  FFileName := Value;
  if Vault = nil then
    Exit;

  Search := Vault.FindFirst(Value, VAULT_FATTR_ANY_FILE, 0);
  if Search <> -1 then
  begin
    Attributes := Vault.GetSearchResultAttributes(Search);
    Caption := Format(Caption, [FileName]);
    edtFileName.Text := ExtractFileNameEx(Value, chr(Vault.PathSeparator));
    edtLocation.Text := ExtractFilePathEx(Value, chr(Vault.PathSeparator)) + chr(Vault.PathSeparator);
    edtSize.Text := Format('%s (%.0n bytes)',
          [FileSizeToStr(Vault.GetSearchResultSize(Search)), Vault.GetSearchResultSize(Search) * 1.0]);
    edtCreated.Text := DateTimeToStr(Vault.GetSearchResultCreationTime(Search));
    edtModified.Text := DateTimeToStr(Vault.GetSearchResultModificationTime(Search));
    edtAccessed.Text := DateTimeToStr(Vault.GetSearchResultLastAccessTime(Search));

    tabLink.TabVisible := (FAttributes and VAULT_FATTR_SYMLINK) <> 0;
    if (FAttributes and VAULT_FATTR_SYMLINK) <> 0 then
      begin
        DestinationName := Vault.GetSearchResultLinkDestination(Search);
        edTarget.Text := DestinationName;
      end;

    Vault.FindClose(Search);

    btnAdvanced.Enabled := ((FAttributes and VAULT_FATTR_FILE) <> 0) and not Vault.ReadOnly;

    chbReadOnly.Enabled := not Vault.ReadOnly;
    chbArchive.Enabled := not Vault.ReadOnly;
    chbHidden.Enabled := not Vault.ReadOnly;
    chbSystem.Enabled := not Vault.ReadOnly;
    chbTemporary.Enabled := not Vault.ReadOnly;

    edtFileName.Enabled := not Vault.ReadOnly;
  end;
end;

procedure TFormFileprops.tabLinkShow(Sender: TObject);
begin
  imgIcon2.Picture.Icon := imgIcon.Picture.Icon;
  edFileName2.Text := edtFileName.Text;
end;

procedure TFormFileprops.UpdateTargetType;
var
  Attr: LongWord;
begin
  if Vault = nil then
    Exit;
  try
    Attr := Vault.GetFileAttributes(edTarget.Text);
    if (Attr and VAULT_FATTR_SYMLINK) <> 0 then
      dlbTargetType.Caption := 'Symbolic Link'
    else if (Attr and VAULT_FATTR_DIRECTORY) <> 0 then
      dlbTargetType.Caption := 'Folder'
    else
      dlbTargetType.Caption := 'File';
  except
    dlbTargetType.Caption := 'Unknown (link destination not found)';
  end;
end;

procedure TFormFileprops.btnChangeTargetClick(Sender: TObject);
begin
  with TFormVaultselectfile.Create(Self) do
    try
      Vault := FVault;
      FileName := edTarget.Text;
      if ShowModal = mrOK then
      begin
        edTarget.Text := FileName;
      end;
    finally
      Free;
    end;
end;

procedure TFormFileprops.edTargetChange(Sender: TObject);
begin
  UpdateTargetType;
end;

end.

