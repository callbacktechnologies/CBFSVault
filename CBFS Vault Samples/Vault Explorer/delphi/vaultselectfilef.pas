unit vaultselectfilef;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ToolWin, ImgList, StdCtrls,
  CBVCore, CBVCBVault, CBVConstants;

type
  TFormVaultselectfile = class(TForm)
    Toolbar: TToolBar;
    tbtnUp: TToolButton;
    ToolButton3: TToolButton;
    tbtnLargeIcons: TToolButton;
    tbtnSmallIcons: TToolButton;
    tbtnList: TToolButton;
    tbtnDetails: TToolButton;
    lvFiles: TListView;
    edFileName: TEdit;
    lbFileName: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    procedure lvFilesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FormCreate(Sender: TObject);
    procedure lvFilesDblClick(Sender: TObject);
    procedure ViewModeChange(Sender: TObject);
    procedure tbtnUpClick(Sender: TObject);
    procedure lvFilesDeletion(Sender: TObject; Item: TListItem);
  private
    FVault: TcbvCBVault;
    FPath: WideString;

    function GetFileName: WideString;
    procedure SetFileName(const Value: WideString);
    procedure SetVault(const Value: TcbvCBVault);

    procedure UpdateFiles;
    procedure UpdateViewMode;
  public
    property Vault: TcbvCBVault read FVault write SetVault;
    property FileName: WideString read GetFileName write SetFileName;
  end;

var
FormVaultselectfile :TFormVaultselectfile;

implementation

uses
  vaultexplorerf;

{$R *.dfm}

{ TFormVaultselectfile }

var
  LastViewStyle: TViewStyle;

procedure TFormVaultselectfile.FormCreate(Sender: TObject);
begin
  lvFiles.SmallImages := FormVaultExplorer.iglFilesSmall;
  lvFiles.LargeImages := FormVaultExplorer.iglFilesLarge;
  Toolbar.Images := FormVaultExplorer.iglToolbar;
  lvFiles.ViewStyle := LastViewStyle;
  UpdateViewMode;
end;

procedure TFormVaultselectfile.lvFilesChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  if Change = ctState then
  begin
    if lvFiles.Selected <> nil then
      edFileName.Text := FPath + chr(Vault.PathSeparator) + lvFiles.Selected.Caption
    else
      edFileName.Text := FPath;
  end;
end;

function TFormVaultselectfile.GetFileName: WideString;
begin
  Result := edFileName.Text;
end;

procedure TFormVaultselectfile.SetFileName(const Value: WideString);
var
  i: Integer;
  FileName: WideString;
begin
  if not Assigned(Vault) then
    Exit;

  i := LastDelimiter(chr(Vault.PathSeparator), Value);
  if i > 0 then
  begin
    FPath := Copy(Value, 1, i - 1);
    FileName := Copy(Value, i + 1, MaxInt);
  end
  else
    Exit;

  UpdateFiles;
  for i := 0 to lvFiles.Items.Count - 1 do
    if lvFiles.Items[i].Caption = FileName then
    begin
      lvFiles.Items[i].Selected := true;
      Break;
    end;
end;

procedure TFormVaultselectfile.SetVault(const Value: TcbvCBVault);
begin
  FVault := Value;
  FPath := '';
  UpdateFiles;
end;

procedure TFormVaultselectfile.tbtnUpClick(Sender: TObject);
var
  i: Integer;
begin
  i := LastDelimiter(chr(Vault.PathSeparator), FPath);
  if i > 0 then
    FPath := Copy(FPath, 1, i - 1)
  else
    FPath := '';

  UpdateFiles;
end;

procedure TFormVaultselectfile.UpdateFiles;
begin
  tbtnUp.Enabled := (FPath <> '');
  vaultexplorerf.UpdateFiles(lvFiles, FVault, FPath, '');
end;

procedure TFormVaultselectfile.lvFilesDblClick(Sender: TObject);
begin
  if (lvFiles.Selected <> nil) and
     ((PListFileInfo(lvFiles.Selected.Data)^.Attributes and VAULT_FATTR_DIRECTORY) <> 0) then
  begin
    FPath := FPath + chr(Vault.PathSeparator) + lvFiles.Selected.Caption;
    UpdateFiles;
  end;
end;

procedure TFormVaultselectfile.UpdateViewMode;
var
  vs: Integer;
begin
  vs := Integer(lvFiles.ViewStyle);
  tbtnLargeIcons.Down := (vs = tbtnLargeIcons.Tag);
  tbtnSmallIcons.Down := (vs = tbtnSmallIcons.Tag);
  tbtnList.Down := (vs = tbtnList.Tag);
  tbtnDetails.Down := (vs = tbtnDetails.Tag);
end;

procedure TFormVaultselectfile.ViewModeChange(Sender: TObject);
begin
  lvFiles.ViewStyle := TViewStyle((Sender as TToolButton).Tag);
  LastViewStyle := lvFiles.ViewStyle;
  UpdateViewMode;
end;

procedure TFormVaultselectfile.lvFilesDeletion(Sender: TObject;
  Item: TListItem);
begin
  FreeFileInfoData(Item.Data);
  Item.Data := nil;
end;

end.
