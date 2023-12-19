unit vaultselectfolderf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ImgList, StdCtrls,
  cbvcore, cbvcbvault, cbvconstants;

type
  TFormVaultselectfolder = class(TForm)
    tvFolders: TTreeView;
    btnOK: TButton;
    btnCancel: TButton;
    lbTitle: TLabel;
    procedure tvFoldersExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure tvFoldersDeletion(Sender: TObject; Node: TTreeNode);
    procedure tvFoldersCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function AddFolder(Vault: TcbvCBVault; Parent: TTreeNode;
      DisplayName, Path: WideString; Attributes: LongWord): TTreeNode;
    procedure ExpandFolder(Node: TTreeNode);

    function GetSelectedFolder: WideString;
    function GetSelectedVault: TcbvCBVault;

    procedure SetSelectedFolder(const Value: WideString);
    procedure SetSelectedVault(const Value: TcbvCBVault);

    function GetNodePath(Node: TTreeNode): WideString;
    function GetNodeRoot(Node: TTreeNode): TTreeNode;
    function GetNodeVault(Node: TTreeNode): TcbvCBVault;
    function GetTitle: string;
    procedure SetTitle(const Value: string);
  public
    procedure AddVault(Vault: TcbvCBVault);
    procedure Clear;
    procedure UpdateFolder(Node: TTreeNode);

    property Folder: WideString read GetSelectedFolder write SetSelectedFolder;
    property Vault: TcbvCBVault read GetSelectedVault write SetSelectedVault;

    property Title: string read GetTitle write SetTitle;
  end;

var
FormVaultselectfolder :TFormVaultselectfolder;

implementation

uses
  vaultexplorerf;

{$R *.dfm}

{ TFormVaultselectfolder }

function TFormVaultselectfolder.AddFolder(Vault: TcbvCBVault; Parent: TTreeNode;
  DisplayName, Path: WideString; Attributes: LongWord): TTreeNode;
var
  Search: Int64;
  SearchResult, HasChildren: Boolean;
  Link: String;
  Attr: Integer;
begin
  Result := tvFolders.Items.AddChild(Parent, DisplayName);
  Result.Data := AllocWideChars(Path);
  Result.ImageIndex := 1;
  HasChildren := false;
  if (Attributes and VAULT_FATTR_SYMLINK) <> 0 then
    Result.OverlayIndex := 0;

  Search := Vault.FindFirst(Path + chr(Vault.PathSeparator) + '*', VAULT_FATTR_DIRECTORY or VAULT_FATTR_SYMLINK, 0);
  if Search <> -1 then
    try
      SearchResult := true;
      while SearchResult do
      begin
        if Vault.GetSearchResultAttributes(Search) and VAULT_FATTR_SYMLINK <> 0 then
          try
            Link := Vault.ResolveLink(Vault.GetSearchResultFullName(Search), true);
            Attr := Vault.GetFileAttributes(Link);
            Vault.SetFileAttributes(Vault.GetSearchResultFullName(Search),
              (Vault.GetSearchResultAttributes(Search) and not (VAULT_FATTR_FILE or VAULT_FATTR_DIRECTORY)) or
              (Attr and (VAULT_FATTR_FILE or VAULT_FATTR_DIRECTORY)));
          except;
          end;

        if Vault.GetSearchResultAttributes(Search) and VAULT_FATTR_DIRECTORY <> 0 then
        begin
          HasChildren := true;
          Break;
        end;

        SearchResult := Vault.FindNext(Search);
      end;
    finally
      Vault.FindClose(Search);
    end;

  Result.HasChildren := HasChildren;
end;

procedure TFormVaultselectfolder.AddVault(Vault: TcbvCBVault);
var
  Root: TTreeNode;
  Search: Int64;
begin
  Root := tvFolders.Items.AddChild(nil, ChangeFileExt(ExtractFileName(Vault.VaultFile), ''));
  Root.Data := Vault;
  Search := Vault.FindFirst(chr(Vault.PathSeparator) + '*', VAULT_FATTR_DIRECTORY, 0);
  Root.HasChildren := Search <> -1;
  Vault.FindClose(Search);
  Root.ImageIndex := 2;
  Root.SelectedIndex := 2;
  Root.Expand(False);
  Root.Selected := true;
end;

procedure TFormVaultselectfolder.Clear;
var
  i: Integer;
begin
  for i := 0 to tvFolders.Items.Count - 1 do
    tvFolders.Items[i].Data := nil;

  tvFolders.Items.Clear;
end;

procedure TFormVaultselectfolder.ExpandFolder(Node: TTreeNode);
var
  Vault: TcbvCBVault;
  NodePath, Link: String;
  Search: Int64;
  SearchResult: Boolean;
  Attr: Integer;
begin
  Vault := GetNodeVault(Node);
  NodePath := GetNodePath(Node);

  Search := Vault.FindFirst(NodePath + chr(Vault.PathSeparator) + '*', VAULT_FATTR_DIRECTORY or VAULT_FATTR_SYMLINK, 0);
  if Search <> -1 then
  begin
    SearchResult := true;
    tvFolders.Items.BeginUpdate;
    try
      while SearchResult do
        begin
          if Vault.GetSearchResultAttributes(Search) and VAULT_FATTR_SYMLINK <> 0 then
          begin
            try
              Link := Vault.ResolveLink(Vault.GetSearchResultFullName(Search), true);
              Attr := Vault.GetFileAttributes(Link);
              Vault.SetFileAttributes(Vault.GetSearchResultFullName(Search),
                (Vault.GetSearchResultAttributes(Search) and not (VAULT_FATTR_FILE or VAULT_FATTR_DIRECTORY)) or
                (Attr and (VAULT_FATTR_FILE or VAULT_FATTR_DIRECTORY)));
            except;
            end;
          end
          else
            Link := Vault.GetSearchResultFullName(Search);

          if Vault.GetSearchResultAttributes(Search) and VAULT_FATTR_DIRECTORY <> 0 then
            AddFolder(Vault, Node, Vault.GetSearchResultName(Search), Link, Vault.GetSearchResultAttributes(Search));
          SearchResult := Vault.FindNext(Search);
        end;
    finally
      Vault.FindClose(Search);
      tvFolders.Items.EndUpdate;
    end;
  end;
end;

procedure TFormVaultselectfolder.FormCreate(Sender: TObject);
begin
  tvFolders.Images := FormVaultExplorer.iglFilesSmall;
end;

procedure TFormVaultselectfolder.FormDestroy(Sender: TObject);
begin
  Clear;
end;

function TFormVaultselectfolder.GetNodePath(Node: TTreeNode): WideString;
begin
  if not Assigned(Node) or
     not Assigned(Node.Data) or
     not Assigned(Node.Parent) then
    Result := ''
  else
    Result := PWideChar(Node.Data);
end;

function TFormVaultselectfolder.GetNodeRoot(Node: TTreeNode): TTreeNode;
begin
  Result := Node;
  if Result = nil then
    Exit;

  while Result.Parent <> nil do
    Result := Result.Parent;
end;

function TFormVaultselectfolder.GetNodeVault(
  Node: TTreeNode): TcbvCBVault;
begin
  Node := GetNodeRoot(Node);
  if Node <> nil then
    Result := TcbvCBVault(Node.Data)
  else
    Result := nil;
end;

function TFormVaultselectfolder.GetSelectedFolder: WideString;
begin
  Result := GetNodePath(tvFolders.Selected);
end;

function TFormVaultselectfolder.GetSelectedVault: TcbvCBVault;
begin
  Result := GetNodeVault(tvFolders.Selected);
end;

function TFormVaultselectfolder.GetTitle: string;
begin
  Result := lbTitle.Caption;
end;

procedure TFormVaultselectfolder.SetSelectedFolder(
  const Value: WideString);

var
  FVault: TcbvCBVault;

  function InternalSelect(Node: TTreeNode): Boolean;
  var
    i: Integer;
    Path: WideString;
  begin
    Result := false;
    Path := GetNodePath(Node);
    for i := 0 to Node.Count - 1 do
    begin
      Path := GetNodePath(Node.Item[i]);
      if Path = Value then
      begin
        Node.Item[i].Expand(False);
        Node.Item[i].Selected := true;
        Result := true;
        Exit;
      end;

      Path := Path + chr(FVault.PathSeparator);
      if (Length(Path) < Length(Value)) and
         (Copy(Value, 1, Length(Path)) = Path) then
      begin
        Node.Item[i].Expand(False);
        Result := InternalSelect(Node.Item[i]);
        if Result then
          Exit
        else
          Node.Item[i].Collapse(True);
      end;
    end;
  end;

var
  RootNode: TTreeNode;
begin
  // check if any vault selected
  FVault := Vault;
  if FVault = nil then
    Exit;

  RootNode := GetNodeRoot(tvFolders.Selected);
  if RootNode = nil then
    Exit;

  if (Value = '') or (Value = chr(FVault.PathSeparator)) then
  begin
    RootNode.Selected := true;
    Exit;
  end;

  tvFolders.Items.BeginUpdate;
  try
    InternalSelect(RootNode);
  finally
    tvFolders.Items.EndUpdate;
  end;
end;

procedure TFormVaultselectfolder.SetSelectedVault(
  const Value: TcbvCBVault);
var
  Node: TTreeNode;
begin
  Node := tvFolders.Items.GetFirstNode;
  while Node <> nil do
  begin
    if GetNodeVault(Node) = Value then
    begin
      Node.Selected := true;
      Break;
    end;

    Node := Node.getNextSibling;
  end;
end;

procedure TFormVaultselectfolder.SetTitle(const Value: string);
begin
  lbTitle.Caption := Value;
end;

procedure TFormVaultselectfolder.tvFoldersCollapsing(Sender: TObject;
  Node: TTreeNode; var AllowCollapse: Boolean);
begin
  AllowCollapse := Node.Parent <> nil;
end;

procedure TFormVaultselectfolder.tvFoldersDeletion(Sender: TObject;
  Node: TTreeNode);
begin
  FreeMem(Node.Data);
end;

procedure TFormVaultselectfolder.tvFoldersExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
begin
  if Node.HasChildren and (Node.Count = 0) then
    ExpandFolder(Node);

  AllowExpansion := true;
end;

procedure TFormVaultselectfolder.UpdateFolder(Node: TTreeNode);
var
  Expanded: Boolean;
begin
  tvFolders.Items.BeginUpdate;
  try
    Expanded := Node.Expanded;
    Node.DeleteChildren;
    ExpandFolder(Node);
    if Expanded or (Node.Parent = nil) then
      Node.Expand(False);
  finally
    tvFolders.Items.EndUpdate;
  end;
end;

end.

