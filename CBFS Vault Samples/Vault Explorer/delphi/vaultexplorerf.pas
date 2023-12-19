(*
 * CBFS Vault 2022 Delphi Edition - Sample Project
 *
 * This sample project demonstrates the usage of CBFS Vault in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.callback.com/cbfsvault
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 *)
unit vaultexplorerf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, Menus, ToolWin, ActnList, ImgList, ShlObj,
  StdCtrls, AppEvnts, Registry, StrRes,
//  System.ImageList, System.Actions, System.UITypes,
  vaultselectfolderf, linkf, openvaultf,
  cbvcore, cbvcbvault, cbvconstants;

type
  TNodeState = record
    Path: String;
    Expanded: Boolean;
  end;

  TNodeStates = array of TNodeState;

  TFormVaultExplorer = class(TForm)
    mnuMain: TMainMenu;
    tvFolders: TTreeView;
    tbrMain: TToolBar;
    mniFile: TMenuItem;
    Splitter1: TSplitter;
    lvFiles: TListView;
    stbMain: TStatusBar;
    aclMain: TActionList;
    mniView: TMenuItem;
    mniTools: TMenuItem;
    mniHelp: TMenuItem;
    actNewVault: TAction;
    mniNew: TMenuItem;
    actOpenVault: TAction;
    actCloseVault: TAction;
    actNewFolder: TAction;
    actDelete: TAction;
    actRename: TAction;
    actProperties: TAction;
    mniOpen: TMenuItem;
    N1: TMenuItem;
    mniClose: TMenuItem;
    iglFilesSmall: TImageList;
    actViewLargeIcons: TAction;
    actViewSmallIcons: TAction;
    actViewList: TAction;
    actViewDetails: TAction;
    mniLargeIcons: TMenuItem;
    mniSmallIcons: TMenuItem;
    mniList: TMenuItem;
    mniDetails: TMenuItem;
    iglFilesLarge: TImageList;
    N2: TMenuItem;
    mniRename: TMenuItem;
    N3: TMenuItem;
    mniNewFolder: TMenuItem;
    mniDelete: TMenuItem;
    actImportFile: TAction;
    actImportByMask: TAction;
    actExportFile: TAction;
    actExportByMask: TAction;
    mniImportFile: TMenuItem;
    mniImportByMask: TMenuItem;
    mniExportFile: TMenuItem;
    mniExportByMask: TMenuItem;
    dlgSaveFile: TSaveDialog;
    dlgOpenFile: TOpenDialog;
    actRefresh: TAction;
    N5: TMenuItem;
    Refresh1: TMenuItem;
    actAbout: TAction;
    About1: TMenuItem;
    appEvents: TApplicationEvents;
    N4: TMenuItem;
    mniProperties: TMenuItem;
    mnuPopup: TPopupMenu;
    Delete1: TMenuItem;
    actOpen: TAction;
    Open1: TMenuItem;
    N6: TMenuItem;
    Rename1: TMenuItem;
    N7: TMenuItem;
    Properties2: TMenuItem;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    iglToolbar: TImageList;
    ToolButton3: TToolButton;
    actUp: TAction;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    actVaultProps: TAction;
    actCheckAndRepair: TAction;
    N8: TMenuItem;
    CheckRepair1: TMenuItem;
    NewFolder1: TMenuItem;
    N9: TMenuItem;
    actNewLink: TAction;
    NewLink1: TMenuItem;
    mniEdit: TMenuItem;
    actSelectAll: TAction;
    mniSelectAll: TMenuItem;
    actInvertSelection: TAction;
    mniInvertSelection: TMenuItem;
    N10: TMenuItem;
    actCopy: TAction;
    actPaste: TAction;
    actCut: TAction;
    mniCut: TMenuItem;
    mniCopy: TMenuItem;
    mniPaste: TMenuItem;
    actCopyToFolder: TAction;
    actMoveToFolder: TAction;
    N11: TMenuItem;
    mniCopyToFolder: TMenuItem;
    mniMoveToFolder: TMenuItem;
    actCompact: TAction;
    Compact1: TMenuItem;
    dlgOpenVault: TOpenDialog;
    actCloseAllVaults: TAction;
    mniNewLink: TMenuItem;
    actExit: TAction;
    mniExit: TMenuItem;
    CloseAll1: TMenuItem;
    mnuTreePopup: TPopupMenu;
    Close1: TMenuItem;
    NewFolder2: TMenuItem;
    NewLink2: TMenuItem;
    Properties1: TMenuItem;
    N12: TMenuItem;
    N13: TMenuItem;
    Delete2: TMenuItem;
    Rename2: TMenuItem;
    N14: TMenuItem;
    actFilter: TAction;
    ToolButton15: TToolButton;
    cmbQuery: TComboBox;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    Filter1: TMenuItem;
    N15: TMenuItem;
    procedure actOpenVaultExecute(Sender: TObject);
    procedure actCloseVaultExecute(Sender: TObject);
    procedure actCloseVaultUpdate(Sender: TObject);
    procedure tvFoldersExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure tvFoldersChange(Sender: TObject; Node: TTreeNode);
    procedure actViewModeUpdate(Sender: TObject);
    procedure actViewModeExecute(Sender: TObject);
    procedure tvFoldersEdited(Sender: TObject; Node: TTreeNode;
      var S: String);
    procedure actRenameExecute(Sender: TObject);
    procedure tvFoldersDeletion(Sender: TObject; Node: TTreeNode);
    procedure actNewFolderUpdate(Sender: TObject);
    procedure actNewFolderExecute(Sender: TObject);
    procedure lvFilesEdited(Sender: TObject; Item: TListItem;
      var S: String);
    procedure actDeleteUpdate(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure tvFoldersEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure actExportFileUpdate(Sender: TObject);
    procedure actExportFileExecute(Sender: TObject);
    procedure OnVaultProgress(Sender: TObject;
      Operation: Integer; const FileName: string;
      Progress, Total: Integer;
      CanStop: Boolean; var Stop: Boolean);
    procedure OnFilePasswordNeeded(Sender: TObject; const FileName: String;
      var Password: String; var TTLInCache: Integer; var ResultCode: Integer);
    procedure lvFilesDblClick(Sender: TObject);
    procedure actNewVaultExecute(Sender: TObject);
    procedure actImportFileUpdate(Sender: TObject);
    procedure actImportFileExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lvFilesResize(Sender: TObject);
    procedure actImportByMaskUpdate(Sender: TObject);
    procedure actImportByMaskExecute(Sender: TObject);
    procedure tvFoldersCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure actRefreshExecute(Sender: TObject);
    procedure actRefreshUpdate(Sender: TObject);
    procedure actExportByMaskUpdate(Sender: TObject);
    procedure actExportByMaskExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure appEventsException(Sender: TObject; E: Exception);
    procedure actPropertiesUpdate(Sender: TObject);
    procedure actPropertiesExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actOpenUpdate(Sender: TObject);
    procedure tvFoldersMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure actUpUpdate(Sender: TObject);
    procedure actUpExecute(Sender: TObject);
    procedure actVaultPropsUpdate(Sender: TObject);
    procedure actVaultPropsExecute(Sender: TObject);
    procedure actCheckAndRepairExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actNewLinkExecute(Sender: TObject);
    procedure actNewLinkUpdate(Sender: TObject);
    procedure lvFilesDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure tvFoldersDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lvFilesDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure tvFoldersDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure actSelectAllExecute(Sender: TObject);
    procedure actSelectAllUpdate(Sender: TObject);
    procedure actInvertSelectionExecute(Sender: TObject);
    procedure actInvertSelectionUpdate(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actCopyUpdate(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actPasteUpdate(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure actCutUpdate(Sender: TObject);
    procedure actCopyToFolderExecute(Sender: TObject);
    procedure actCopyToFolderUpdate(Sender: TObject);
    procedure actMoveToFolderExecute(Sender: TObject);
    procedure actMoveToFolderUpdate(Sender: TObject);
    procedure actCompactExecute(Sender: TObject);
    procedure actCompactUpdate(Sender: TObject);
    procedure actCloseAllVaultsExecute(Sender: TObject);
    procedure actCloseAllVaultsUpdate(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure lvFilesColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvFilesCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure lvFilesDeletion(Sender: TObject; Item: TListItem);
    procedure actFilterExecute(Sender: TObject);
    procedure actFilterUpdate(Sender: TObject);
  private
    // data for active folder
    FVault: TcbvCBVault;
    FPath: String;
    FRoot: TTreeNode;
    FNode: TTreeNode;

    FOperation: Integer;
    FOperationStart: LongWord;

    DeleteConfirmationAsked: Boolean;
    CopyFileOptionsSeted: Boolean;
    MoveFileOptionsSeted: Boolean;

    BufferCutFiles: Boolean;
    BufferVault: TcbvCBVault;
    BufferFiles: array of String;

    SortSectionToCompare: Integer;
    SortInverted: Boolean;

    FilterActive: Boolean;

    procedure ClearBuffer;

    function AddFolder(Parent: TTreeNode; DisplayName,
      Path: String; Attributes: LongWord): TTreeNode;
    procedure ExpandFolder(Node: TTreeNode);
    procedure UpdateFolder(Node: TTreeNode);
    procedure UpdateFiles(Node: TTreeNode);

    procedure AcceptFiles(var msg: TMessage); message WM_DROPFILES;

    procedure FileToVault(const FileName, VaultFileName: String);
    function SetFilesEncryptionAndCompression(Vault: TcbvCBVault;
      const FileName: String; var ApplyToAllFiles: Boolean): Boolean;
    procedure ClearFileOptions(Vault: TcbvCBVault);

    procedure CopyFileToVault(SourceVault, DestVault: TcbvCBVault;
      const SourceFileName, DestFileName: String);

    procedure DeleteFiles(Vault: TcbvCBVault; const FileName: String; Attributes: LongWord);
    procedure CopyFiles(SourceVault, DestVault: TcbvCBVault; const SourceFileName, NewPath: String);
    function MoveFile(Vault: TcbvCBVault; const OldFileName, NewFileName: String): Boolean;
    procedure MoveFiles(SourceVault, DestVault: TcbvCBVault; const SourceFileName, NewPath: String);

    function FindNode(RootNode: TTreeNode; const Path: String): TTreeNode;
    function GetNodePath(Node: TTreeNode): String;
    function GetNodeFullPath(Node: TTreeNode): String;
    function GetNodeRoot(Node: TTreeNode): TTreeNode;
    function GetNodeVault(Node: TTreeNode): TcbvCBVault;
    procedure SelectNode(const Path: String);

    procedure SaveNodes(Node: TTreeNode; var NodeStates: TNodeStates);
    procedure RestoreAndUpdateNodes(Node: TTreeNode; const NodeStates: TNodeStates);
    procedure UpdateNodes(Node: TTreeNode);

    procedure SaveOpenVaults;
  public
    procedure OpenVault(const FileName: String;
      Password: String = ''; ReadOnly: Boolean = false;
      UseJournaling: Boolean = false; UseAccessTime: Boolean = false;
      AutoCompact: LongWord = 0; QuietMode: Boolean = false);

    procedure CloseActiveVault;
    procedure CloseAllVaults;
    procedure Refresh;
  end;

  TListFileInfo = record
    FileName: PWideChar;
    Attributes: LongWord;
    FileSize: Int64;
    ModificationTime: TDateTime;
  end;

  PListFileInfo = ^TListFileInfo;

var
  FormVaultExplorer: TFormVaultExplorer;
  LoadLastVaults: Boolean = true;
  RegKey: AnsiString;
  RegistrationKeyOK: Boolean = false;

const
  icoOpenedFolder = 0;
  icoFolder = 1;
  icoDisk = 2;
  icoFile = 3;
  icoShortcut = 4;
  icoSortAsc = 5;
  icoSortDesc = 6;

  scFileName = 0;
  scFileSize = 1;
  scModificationTime = 2;
  scAttributes = 3;

function AllocWideChars(Str: String): PWideChar;
function FileSizeToStr(Size: Int64): String;

function ExtractFileNameEx(const FileName: string; PathSeparator: string): string;
function ExtractFilePathEx(const FileName: string; PathSeparator: string): string;

function ChooseFolder(Owner: TCustomForm; var Directory: String;
  Caption: String): Boolean;

procedure ShowRegistrationKeyWarning;

procedure FreeFileInfoData(Data: PListFileInfo);
procedure UpdateFiles(lvFiles: TListView; Vault: TcbvCBVault;
  Path: String; Query: String);

implementation

uses
  ShellApi,
  wizardf, passwordf, progressf, importfilef, importf, exportf, aboutf, filepropsf, vaultpropsf;

{$R *.DFM}

function AllocWideChars(Str: String): PWideChar;
var
  Len: Integer;
  tmp : WideString;
begin
  Len := (Length(Str) + 1) * SizeOf(WideChar);
  Result := PWideChar(AllocMem(Len));
  tmp := WideString(Str);
  Move(PWideChar(tmp)^, Result^, Len);
end;

function WideCompareText(const S1, S2: String): Integer;
var
  P1, P2: PWideChar;
begin
  Result := 0;
  P1 := @S1[1];
  P2 := @S2[1];
  while (P1^ <> #0) and (P2^ <> #0) and (P2^ = P1^) do
  begin
    inc(P1);
    Inc(P2);
  end;
  if P1^ = P2^ then
    Result := 0
  else
  if P1^ < P2^ then
    Result := -1
  else
  if P1^ > P2^ then
    Result := 1;
end;

function FileSizeToStr(Size: Int64): String;
begin
  if Size < 1024 then
    Result := Format('%d B ', [Size])
  else if Size < 2 * 1024 * 1024 then
    Result := Format('%f KB ', [Size / 1024])
  else if Size < Int64(2) * 1024 * 1024 * 1024 then
    Result := Format('%f MB ', [Size / (1024 * 1024)])
  else
    Result := Format('%f GB ', [Size / (1024 * 1024 * 1024)]);
end;

function GetPassword(Vault: Boolean; ObjectName: String;
  var Password: String): Boolean;
const
  ObjectTypes: array[Boolean] of String = ('file', 'vault');
begin
  Application.CreateForm(TFormPassword, FormPassword);
  try
    FormPassword.lbPrompt.Caption := Format(PASSWORD_Prompt,
      [ObjectTypes[Vault], ObjectName]);
    FormPassword.edPassword.Text := Password;
    Result := FormPassword.ShowModal = mrOK;
    if Result then
      Password := FormPassword.edPassword.Text;
  finally
    FreeAndNil(FormPassword);
  end;
end;

function ExtractFileNameEx(const FileName: string; PathSeparator: string): string;
var
  I: Integer;
begin
  I := LastDelimiter(PathSeparator, FileName);
  Result := Copy(FileName, I + 1, MaxInt);
end;

function ExtractFilePathEx(const FileName: string; PathSeparator: string): string;
var
  I: Integer;
begin
  I := LastDelimiter(PathSeparator, FileName);
  Result := Copy(FileName, 1, I - 1);
end;

function NormalizeDir(const DirName: String): String;
begin
  Result := DirName;
  if (Result <> '') and not (AnsiLastChar(Result)^ in [':', '\']) then
  begin
    if (Length(Result) = 1) and (UpCase(Result[1]) in ['A'..'Z']) then
      Result := Result + ':\'
    else
      Result := Result + '\';
  end;
end;

function ChooseFolder(Owner: TCustomForm; var Directory: String;
  Caption: string): Boolean;
var
  BrowseInfo: TBrowseInfo;
  ItemIDList: PItemIDList;
  ResultDir: array [0..MAX_PATH] of Char;
  ActiveWindow: HWnd;
  WindowList: Pointer;

  function BrowseCallbackProc(hwnd: THandle; uMsg: UINT;
    lParam: Integer; lpData: Pointer): Integer; stdcall;
  begin
    if (uMsg = BFFM_INITIALIZED) and Assigned(lpData) then
      SendMessage(hwnd, BFFM_SETSELECTION, 1, Integer(lpData));

    Result := 0;
  end;

begin
  with BrowseInfo do
  begin
    if Owner <> nil then
      hwndOwner := Owner.Handle
    else
      hwndOwner := 0;

    pidlRoot := nil;
    pszDisplayName := @ResultDir;
    lpszTitle := PChar(Caption);
    ulFlags := BIF_RETURNONLYFSDIRS;
    lpfn := @BrowseCallbackProc;
    lParam := Integer(PChar(Directory));
    iImage := 0;
  end;

  ActiveWindow := GetActiveWindow;
  WindowList := DisableTaskWindows(0);
  try
    ItemIDList := SHBrowseForFolder(BrowseInfo);
  finally
    EnableTaskWindows(WindowList);
    SetActiveWindow(ActiveWindow);
  end;

  Result := ItemIDList <> nil;
  if Result then
  begin
    {$ifdef D_6_UP}
    {$WARN SYMBOL_PLATFORM OFF}
    {$endif}
    Win32Check(SHGetPathFromIDList(ItemIDList, ResultDir));
    {$ifdef D_6_UP}
    {$WARN SYMBOL_PLATFORM ON}
    {$endif}
    SetString(Directory, ResultDir, StrLen(ResultDir));
    Directory := NormalizeDir(Directory);
  end
  else
    Directory := '';
end;

procedure ShowRegistrationKeyWarning;
var
  KeyFile: string;
begin
  KeyFile := ChangeFileExt(Application.ExeName, '.key');
  MessageDlg(Format(RegistrationKeyWarning, [KeyFile]),
    mtWarning, [mbOK], 0);
end;

procedure FreeFileInfoData(Data: PListFileInfo);
begin
  if Assigned(Data) then
  begin
    if Assigned(Data^.FileName) then
      FreeMem(Data^.FileName);

    FreeMem(Data);
  end;
end;

procedure UpdateFiles(lvFiles: TListView; Vault: TcbvCBVault;
  Path: String; Query: String);
var
  Item: TListItem;
  Search: Int64;
  AttrStr: String;
  Compression, Attr: Integer;
  Link: String;
  LFI: PListFileInfo;
  SearchResult: Boolean;
begin
  lvFiles.Items.BeginUpdate;
  try
    lvFiles.Items.Clear;
    if not Assigned(Vault) then
      Exit;

    if Query <> '' then
      Search := Vault.FindFirstByQuery(Path, Query, 0)
    else
      Search := Vault.FindFirst(Path + chr(Vault.PathSeparator) + '*', VAULT_FATTR_ANY_FILE,0);
    if Search <> -1 then
      try
        SearchResult := true;
        while SearchResult do
        begin
          Item := lvFiles.Items.Add;
          Item.Caption := Vault.GetSearchResultName(Search);
          if (Vault.GetSearchResultAttributes(Search) and VAULT_FATTR_SYMLINK) <> 0 then
            begin
              Item.OverlayIndex := 0;
              try
                Link := Vault.ResolveLink(Vault.GetSearchResultFullName(Search), true);
                Attr := Vault.GetFileAttributes(Link);
                Vault.SetFileAttributes(Vault.GetSearchResultFullName(Search),
                  (Vault.GetSearchResultAttributes(Search) and not (VAULT_FATTR_FILE or VAULT_FATTR_DIRECTORY)) or
                  (Attr and (VAULT_FATTR_FILE or VAULT_FATTR_DIRECTORY)));
              except
              end;
            end;

          Item.Data := Pointer(Vault.GetSearchResultAttributes(Search));
          GetMem(LFI, SizeOf(TListFileInfo));//Pointer(Search.Attributes);
          Item.Data := LFI;
          LFI.FileName := AllocWideChars(Vault.GetSearchResultName(Search));
          LFI.Attributes := Vault.GetSearchResultAttributes(Search);
          LFI.FileSize := Vault.GetSearchResultSize(Search);
          LFi.ModificationTime := Vault.GetSearchResultModificationTime(Search);

          if (Vault.GetSearchResultAttributes(Search) and VAULT_FATTR_DIRECTORY) <> 0 then
            begin
              Item.ImageIndex := icoFolder;
              if (Vault.GetSearchResultAttributes(Search) and VAULT_FATTR_SYMLINK) <> 0 then
                Item.SubItems.Add(FileSizeToStr(Vault.GetSearchResultSize(Search)))
              else
                Item.SubItems.Add('');
            end
          else
            begin
              Item.ImageIndex := icoFile;
              Item.SubItems.Add(FileSizeToStr(Vault.GetSearchResultSize(Search)));
            end;

          Item.SubItems.Add(
            DateTimeToStr(Vault.GetSearchResultModificationTime(Search)));
          AttrStr := '';
          if (Vault.GetSearchResultAttributes(Search) and VAULT_FATTR_SYMLINK) <> 0 then
            AttrStr := AttrStr + 'L'
          else if (Vault.GetSearchResultAttributes(Search) and VAULT_FATTR_DIRECTORY) <> 0 then
            AttrStr := AttrStr + 'D';
          if (Vault.GetSearchResultAttributes(Search) and VAULT_FATTR_COMPRESSED) <> 0 then
            begin
              AttrStr := AttrStr + 'C';
              Compression := Vault.GetFileCompression(Vault.GetSearchResultFullName(Search));
              AttrStr := AttrStr + IntToStr(Compression);
            end;
          if (Vault.GetSearchResultAttributes(Search) and VAULT_FATTR_ENCRYPTED) <> 0 then
            AttrStr := AttrStr + 'E';
          if (Vault.GetSearchResultAttributes(Search) and VAULT_FATTR_READONLY) <> 0 then
            AttrStr := AttrStr + 'R';
          if (Vault.GetSearchResultAttributes(Search) and VAULT_FATTR_ARCHIVE) <> 0 then
            AttrStr := AttrStr + 'A';
          if (Vault.GetSearchResultAttributes(Search) and VAULT_FATTR_HIDDEN) <> 0 then
            AttrStr := AttrStr + 'H';
          if (Vault.GetSearchResultAttributes(Search) and VAULT_FATTR_SYSTEM) <> 0 then
            AttrStr := AttrStr + 'S';
          if (Vault.GetSearchResultAttributes(Search) and VAULT_FATTR_TEMPORARY) <> 0 then
            AttrStr := AttrStr + 'T';

          if (Vault.GetSearchResultAttributes(Search) and VAULT_FATTR_SYMLINK) <> 0 then
            begin
              Link := Vault.GetSearchResultLinkDestination(Search);
              AttrStr := AttrStr + ' -> ' + Link;
            end;

          Item.SubItems.Add(AttrStr);
          SearchResult := Vault.FindNext(Search);
        end;
      finally
        Vault.FindClose(Search);
      end;
  finally
    lvFiles.Items.EndUpdate;
  end;
end;

procedure TFormVaultExplorer.ExpandFolder(Node: TTreeNode);
var
  Vault: TcbvCBVault;
  NodePath, Link: String;
  Search: Int64;
  SearchResult: Boolean;
  Attr: Integer;
begin
  Vault := GetNodeVault(Node);
  NodePath := GetNodePath(Node);

  Search := Vault.FindFirst(NodePath + chr(Vault.PathSeparator) + '*',VAULT_FATTR_DIRECTORY or VAULT_FATTR_SYMLINK, 0);
  if Search <> -1 then
  begin
    tvFolders.Items.BeginUpdate;
    try
      SearchResult := true;
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
            AddFolder(Node, Vault.GetSearchResultName(Search), Link, Vault.GetSearchResultAttributes(Search));
          SearchResult := Vault.FindNext(Search);
        end;
    finally
      Vault.FindClose(Search);
      tvFolders.Items.EndUpdate;
    end;
  end;
end;

function TFormVaultExplorer.AddFolder(Parent: TTreeNode; DisplayName,
  Path: String; Attributes: LongWord): TTreeNode;
var
  Vault: TcbvCBVault;
  Search: Int64;
  SearchResult, HasChildren: Boolean;
  Link: String;
  Attr: Integer;
begin
  Vault := GetNodeVault(Parent);
  Result := tvFolders.Items.AddChild(Parent, DisplayName);
  Result.Data := AllocWideChars(Path);
  Result.ImageIndex := icoFolder;
  Result.SelectedIndex := icoOpenedFolder;
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

procedure TFormVaultExplorer.UpdateFolder(Node: TTreeNode);
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

  UpdateFiles(Node);
end;

procedure TFormVaultExplorer.UpdateFiles(Node: TTreeNode);
begin
  if Node = nil then
    Exit;

  FNode := Node;
  FRoot := GetNodeRoot(Node);
  FVault := GetNodeVault(FRoot);
  FPath := GetNodePath(Node);

  if FilterActive then
    vaultexplorerf.UpdateFiles(lvFiles, FVault, FPath, cmbQuery.Text)
  else
    vaultexplorerf.UpdateFiles(lvFiles, FVault, FPath, '');

  lvFiles.AlphaSort;
end;

procedure TFormVaultExplorer.actNewVaultExecute(Sender: TObject);
var
  Size: Int64;
  Vault: TcbvCBVault;
begin
  Application.CreateForm(TFormWizard, FormWizard);
  try
    if FormWizard.ShowModal = mrOK then
      begin
        Vault := TcbvCBVault.Create(nil);

        Vault.VaultFile := FormWizard.edtFileName.Text;
        Vault.PageSize  := FormWizard.FPageSize;
        Vault.Logo      := FormWizard.mmLogo.Text;
        if not FormWizard.rbtVariableSize.Checked then
        begin
          Size := StrToIntDef(FormWizard.edtVaultSize.Text, 0);
          if Size = 0 then
            Size := FormWizard.spnVaultSize.Position;

          Vault.VaultSizeMax := Size * 1024 * 1024;
          Vault.VaultSizeMin := Vault.VaultSizeMax;

        end;
        Vault.OpenVault(VAULT_OM_CREATE_ALWAYS, VAULT_JM_NONE);

        if FormWizard.chbEncryption.Checked then
          Vault.UpdateVaultEncryption(VAULT_EM_DEFAULT, '', String(FormWizard.edtPassword.Text));

        //Vault.SetFileCompression('\', crZLib, 0, 0, '');
        //Vault.CurrentSize := Int64(30) * 1024 * 1024 * 1024;

        FreeAndNil(Vault);

        OpenVault(FormWizard.edtFileName.Text, FormWizard.edtPassword.Text);
      end;
  finally
    FreeAndNil(FormWizard);
  end;
end;

procedure TFormVaultExplorer.actVaultPropsExecute(Sender: TObject);
var
  Capacity, FreeSize: Int64;
begin
  Application.CreateForm(TFormVaultprops, FormVaultprops);
  try
    FormVaultprops.Caption := Format(FormVaultprops.Caption,
      [FVault.VaultFile]);
    FormVaultprops.lblLogo.Caption := FVault.Logo;
    FormVaultprops.lblPageSizeValue.Caption := Format('%s(%.0n bytes)',
      [FileSizeToStr(FVault.PageSize), FVault.PageSize * 1.0]);
    if (FVault.VaultSizeMin = FVAult.VaultSizeMax) and (FVault.VaultSizeMin > 0) then
      FormVaultprops.lblTypeValue.Caption := 'Fixed size'
    else
      FormVaultprops.lblTypeValue.Caption := 'Variable size';
    FormVaultprops.lblVaultName.Caption := FRoot.Text;
    Capacity := FVault.VaultSize;
    FreeSize := FVault.VaultFreeSpace;
    FormVaultprops.lblUsedValue.Caption :=
      FileSizeToStr(Capacity - FreeSize);
    FormVaultprops.lblUsedBytes.Caption :=
      Format('%.0n bytes', [(Capacity - FreeSize) * 1.0]);
    FormVaultprops.lblFreeValue.Caption :=
      FileSizeToStr(FreeSize);
    FormVaultprops.lblFreeBytes.Caption :=
      Format('%.0n bytes', [FreeSize * 1.0]);
    FormVaultprops.lblCapaValue.Caption :=
      FileSizeToStr(Capacity);
    FormVaultprops.lblCapaBytes.Caption :=
      Format('%.0n bytes', [Capacity * 1.0]);

    FormVaultprops.FileName := FVault.VaultFile;
    FormVaultprops.Encryption := FVault.VaultEncryption;
    FormVaultprops.Password := FVault.VaultPassword;
    FormVaultprops.btnAdvanced.Enabled := not FVault.ReadOnly;

    if FormVaultprops.ShowModal = mrOK then
    begin
      if (FormVaultprops.Encryption <> FVault.VaultEncryption) or
         (FormVaultprops.Password <> FVault.VaultPassword) then
      begin
        if FormVaultprops.Encryption = VAULT_EM_NONE then
          FVault.UpdateVaultEncryption(FormVaultprops.Encryption, FormVaultprops.Password, '')
        else
          FVault.UpdateVaultEncryption(FormVaultprops.Encryption, FVault.VaultPassword,FormVaultprops.Password);
      end;
    end;
  finally
    FreeAndNil(FormVaultprops);
  end;
end;

procedure TFormVaultExplorer.actVaultPropsUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := FVault.Active;
end;

procedure TFormVaultExplorer.OnFilePasswordNeeded(Sender: TObject;
  const FileName: String; var Password: String; var TTLInCache: Integer;
  var ResultCode: Integer);
begin
  if GetPassword(False, FileName, Password) then
    ResultCode := 0
  else
    ResultCode := VAULT_ERR_INVALID_PASSWORD;
end;

procedure TFormVaultExplorer.OnVaultProgress(Sender: TObject;
  Operation: Integer; const FileName: string;
  Progress, Total: Integer; CanStop: Boolean;
  var Stop: Boolean);
const
  Operations: array [VAULT_PO_FORMATTING ..
    VAULT_PO_COPYING_FILES_FROM_VAULT] of String = (
    'Formating',
    'Checking1',
    'Checking2',
    'Checking3',
    'Checking4',
    'Checking5',
    'Copying file %s',
    'Copying file %s',
    'Page %d corrupted',
    'Page %d orphaned',
    'Compressing',
    'Decompressing',
    'Encrypting',
    'Decrypting',
    'Compacting',
    'Deleting file %s',
    'Calculating size',
    'Copying file %s',
    'Copying file %s'
  );
var
  LastTickCount: LongWord;
begin
  LastTickCount := GetTickCount;
  try
    FormProgress.btnCancel.Enabled := CanStop;
    if not (Operation in [VAULT_PO_PAGE_CORRUPTED, VAULT_PO_PAGE_ORPHANED]) then
      begin
        FormProgress.lblText.Caption :=
          Format(Operations[Operation], [String('')]);
        FormProgress.barProgress.Max := Total;
        FormProgress.barProgress.Position := Progress;
      end
    else
      begin
        FormProgress.lstList.Items.Add(Format(Operations[Operation], [Progress]));
        if not FormProgress.pnlList.Visible then
          begin
            FormProgress.ClientHeight := FormProgress.ClientHeight +
              FormProgress.pnlList.Height;
            FormProgress.pnlList.Visible := true;
          end;
      end;

    if Operation in [VAULT_PO_CHECKING_1, VAULT_PO_CHECKING_2, VAULT_PO_CHECKING_3, VAULT_PO_CHECKING_4,
      VAULT_PO_CHECKING_5, VAULT_PO_PAGE_CORRUPTED, VAULT_PO_PAGE_ORPHANED] then
      FormProgress.FAutoHide := false
    else
      FormProgress.FAutoHide := true;

    if FOperation <> Operation then
      begin
        FOperation := Operation;
        FOperationStart := GetTickCount;
      end;

    if CanStop then
      Stop := FormProgress.FCancel;
    
    if (Total > 0) and FormProgress.FAutoHide and ((Progress >= Total) or Stop) then
      begin
        FormProgress.Hide;
        FormProgress.FAutoHide := true;
        FormProgress.FCancel := false;
        Enabled := true;
        FOperation := -1;
        if FormProgress.pnlList.Visible then
          begin
            FormProgress.lstList.Items.Clear;
            FormProgress.pnlList.Visible := false;
            FormProgress.ClientHeight := FormProgress.ClientHeight -
              FormProgress.pnlList.Height;
          end;
      end
    else if not FormProgress.Visible then
      begin
        if (GetTickCount - FOperationStart > 200) or
          FormProgress.pnlList.Visible then
          begin
            FormProgress.Show;
            FormProgress.BringToFront;
            Enabled := false;
          end;
      end
    else
    if (GetTickCount - LastTickCount) > 200 then
    begin
      LastTickCount := GetTickCount;
      Application.ProcessMessages;
    end;
  except
  end;
end;

procedure TFormVaultExplorer.OpenVault(const FileName: String;
  Password: String = ''; ReadOnly: Boolean = false;
  UseJournaling: Boolean = false; UseAccessTime: Boolean = false;
  AutoCompact: LongWord = 0; QuietMode: Boolean = false);
var
  RootNode: TTreeNode;
  Vault: TcbvCBVault;
  Valid: Boolean;
  Search: Int64;
begin
  RootNode := tvFolders.Items.GetFirstNode;
  while RootNode <> nil do
  begin
    if GetNodeVault(RootNode).VaultFile = FileName then
    begin
      if not QuietMode then
        MessageDlg(Format(VaultAlreadyOpened, [string(FileName)]),
          mtError, [mbOK], 0);

      Exit;
    end;

    RootNode := RootNode.getNextSibling;
  end;

  FRoot := nil;
  Vault := TcbvCBVault.Create(nil);

  Vault.OnProgress := OnVaultProgress;
  Vault.OnFilePasswordNeeded := OnFilePasswordNeeded;
  Vault.VaultFile := FileName;
  Vault.ReadOnly := ReadOnly;
  Vault.AutoCompactAt := AutoCompact;
  Vault.UseAccessTime := UseAccessTime;
  try
    if not Vault.IsValidVault() then
    begin
      if not QuietMode then
        MessageDlg(Format(InvalidVaultFile, [string(FileName)]),
          mtError, [mbOK], 0);
      FreeAndNil(Vault);
      Exit;
    end;
    Vault.OpenVault(VAULT_OM_OPEN_EXISTING, VAULT_JM_NONE);
  except
    on E: Exception do
    begin
      if not QuietMode then
        Application.ShowException(E);

      FreeAndNil(Vault);
      Exit;
    end;
  end;

  if Vault.VaultEncryption <> VAULT_EM_NONE then
  begin
    Valid := false;
    try
      Vault.VaultPassword := Password;
      Valid := true;
    except
      on E: Exception do
        if not (E is EcbvCBVault) or
          (EcbvCBVault(E).Code <> VAULT_ERR_INVALID_PASSWORD) then
          Application.ShowException(E)
    end;

    Password := '';
    if not Valid then
      while GetPassword(True, Vault.VaultFile, Password) do
      try
        Vault.VaultPassword := Password;
        Valid := true;
        Break;
      except
        on E: Exception do
          Application.ShowException(E)
      end;
  end
  else
    Valid := true;

  if Valid then
  begin
    try
      FVault := Vault;
      FRoot := tvFolders.Items.AddChild(nil,
        ChangeFileExt(ExtractFileName(FVault.VaultFile), ''));
      FRoot.Data := Vault;
      Search := -1;
      try
        Search := Vault.FindFirst(String(chr(Vault.PathSeparator)) + '*', VAULT_FATTR_DIRECTORY, 0);
        FRoot.HasChildren := Search <> -1;
      finally
        if Search <> -1 then Vault.FindClose(Search);
      end;
      FRoot.ImageIndex := icoDisk;
      FRoot.SelectedIndex := icoDisk;
      FRoot.Expand(False);
      UpdateFiles(FRoot);
      FRoot.Selected := true;
    except
      on E: Exception do
      begin
        FreeAndNil(Vault);
        if FRoot <> nil then
        begin
          FRoot.Data := nil;
          FRoot.Delete;
        end;

        FVault := nil;
        FRoot := nil;
        FNode := nil;
        FPath := '';

        Application.ShowException(E);
      end;
    end;
  end
  else
    FreeAndNil(Vault);
end;

procedure TFormVaultExplorer.tvFoldersExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
begin
  if Node.HasChildren and (Node.Count = 0) then
    ExpandFolder(Node);

  AllowExpansion := true;
end;

procedure TFormVaultExplorer.tvFoldersChange(Sender: TObject;
  Node: TTreeNode);
begin
  if not tvFolders.Dragging then
    UpdateFiles(Node);
end;

procedure TFormVaultExplorer.tvFoldersEditing(Sender: TObject;
  Node: TTreeNode; var AllowEdit: Boolean);
begin
  AllowEdit := Node <> FRoot;
end;

procedure TFormVaultExplorer.tvFoldersEdited(Sender: TObject;
  Node: TTreeNode; var S: String);
var
  OldName, NewName: String;

  procedure UpdateNodeData(Node: TTreeNode);
  var
    i: Integer;
    FolderName: String;
  begin
    for i := 0 to Node.Count - 1 do
    begin
      FolderName := PWideChar(Node.Item[i].Data);
      FreeMem(Node.Item[i].Data);
      FolderName := NewName + Copy(FolderName, Length(OldName) + 1, MaxInt);
      Node.Item[i].Data := AllocWideChars(FolderName);
      UpdateNodeData(Node.Item[i]);
    end;
  end;

begin
  ClearBuffer;
  OldName := GetNodePath(Node);
  NewName := Char(FVault.PathSeparator) + S;
  if Node.Parent <> nil then
    NewName := GetNodePath(Node.Parent) + NewName;

  try
    FVault.MoveFile(OldName, NewName, false);
  except
    on E: Exception do
    begin
      Application.ShowException(E);
      Exit;
    end;
  end;
  
  FreeMem(PWideChar(Node.Data));
  Node.Data := AllocWideChars(NewName);
  UpdateNodeData(Node);
end;

procedure TFormVaultExplorer.tvFoldersDeletion(Sender: TObject;
  Node: TTreeNode);
begin
  if Assigned(Node.Data) then
    FreeMem(PWideChar(Node.Data));
end;

procedure TFormVaultExplorer.lvFilesEdited(Sender: TObject;
  Item: TListItem; var S: String);
var
  OldName, NewName: String;
  Expanded: Boolean;
begin
  ClearBuffer;
  NewName := FPath + chr(FVault.PathSeparator);
  OldName := NewName + Item.Caption;
  NewName := NewName + S;
  FVault.MoveFile(OldName, NewName, false);
//  UpdateFolder(FNode);

  tvFolders.Items.BeginUpdate;
  try
    Expanded := FNode.Expanded;
    FNode.DeleteChildren;
    ExpandFolder(FNode);
    if Expanded or (FNode.Parent = nil) then
      FNode.Expand(False);
  finally
    tvFolders.Items.EndUpdate;
  end;
end;

procedure TFormVaultExplorer.lvFilesDblClick(Sender: TObject);
begin
  if actOpen.Enabled then
    actOpen.Execute;
end;

procedure TFormVaultExplorer.lvFilesResize(Sender: TObject);
begin
  lvFiles.Arrange(arDefault);
  lvFiles.Repaint;
end;

procedure TFormVaultExplorer.tvFoldersCollapsing(Sender: TObject;
  Node: TTreeNode; var AllowCollapse: Boolean);
begin
  AllowCollapse := Node <> FRoot;
end;

procedure TFormVaultExplorer.tvFoldersMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
begin
  if Button = mbRight then
  begin
    Node := tvFolders.GetNodeAt(X, Y);
    if Node <> nil then
      tvFolders.Selected := Node;
  end;
end;

procedure TFormVaultExplorer.FormCreate(Sender: TObject);
var
  Reg: TRegIniFile;
  FileName: String;
  I: Integer;
begin
  DragAcceptFiles(Handle, true);

  if LoadLastVaults then
  begin
    Reg := TRegIniFile.Create;
    Reg.RootKey := REG_ROOTKEY;
    if Reg.OpenKey(REG_PATH, false) then
    begin
      lvFiles.ViewStyle := TViewStyle(Reg.ReadInteger('', 'ViewStyle', 0));
      Reg.CloseKey;
    end;

    I := 0;
    while Reg.OpenKey(REG_PATH + REG_VAULTS + '\' + IntToStr(I), false) do
    begin
      FileName := Reg.ReadString('', 'FileName', '');
      if FileName <> '' then
      begin
        OpenVault(FileName, '',
          Reg.ReadBool('', 'ReadOnly', false),
          Reg.ReadBool('', 'UseJournaling', false),
          Reg.ReadBool('', 'UseAccessTime', false),
          Reg.ReadInteger('', 'AutoCompact', 25),
          true);
      end;

      Reg.CloseKey;
      Inc(i);
    end;

    Reg.Free;

    tvFolders.Selected := tvFolders.Items.GetFirstNode;
  end;

  iglFilesSmall.Overlay(icoShortcut, 0);
  iglFilesLarge.Overlay(icoShortcut, 0);

  lvFiles.Columns[0].ImageIndex := icoSortAsc;
end;

procedure TFormVaultExplorer.FormDestroy(Sender: TObject);
begin
  SaveOpenVaults;
  CloseAllVaults;
end;

procedure TFormVaultExplorer.AcceptFiles(var msg: TMessage);
var
  i, Count: Integer;
  acFileName: array [0..MAX_PATH] of Char;
  FileName, VaultFileName: String;
  Res, FileOptionsSeted, ApplyToAllFiles: Boolean;
begin
  if not Assigned(FVault) or
     not FVault.Active then
  begin
    DragFinish(msg.WParam);
    Exit;
  end;

  // find out how many files we're accepting
  Count := DragQueryFile(msg.WParam, $FFFFFFFF, acFileName, MAX_PATH);

  FileOptionsSeted := false;
  for i := 0 to Count - 1 do
  begin
    DragQueryFile(msg.WParam, i, acFileName, MAX_PATH);

    FileName := acFileName;
    VaultFileName := FPath + chr(FVault.PathSeparator) + ExtractFileName(FileName);
    if not FileOptionsSeted then
    begin
      if Count = 1 then
      begin
        ApplyToAllFiles := false;
        Res := SetFilesEncryptionAndCompression(FVault, VaultFileName, ApplyToAllFiles);
      end
      else
      begin
        ApplyToAllFiles := true;
        Res := SetFilesEncryptionAndCompression(FVault, VaultFileName + ' and other files', ApplyToAllFiles);
        FileOptionsSeted := ApplyToAllFiles;
      end;

      if not Res then
        Break;
    end;

    FileToVault(FileName, VaultFileName);
  end;

  ClearFileOptions(FVault);
  UpdateFiles(FNode);
  DragFinish(msg.WParam);
end;

procedure TFormVaultExplorer.actAboutExecute(Sender: TObject);
begin
  Application.CreateForm(TFormAbout, FormAbout);
  try
    FormAbout.ShowModal;
  finally
    FreeAndNil(FormAbout);
  end;
end;

procedure TFormVaultExplorer.actCheckAndRepairExecute(Sender: TObject);
var
  Vault: TcbvCBVault;
  RootNode: TTreeNode;
  VaultOK: Boolean;
begin
  dlgOpenVault.Options := dlgOpenVault.Options - [ofReadOnly];
  if not dlgOpenVault.Execute then
    Exit;

  ClearBuffer;
  RootNode := tvFolders.Items.GetFirstNode;
  while RootNode <> nil do
  begin
    Vault := GetNodeVault(RootNode);
    if (Vault <> nil) and
       (Vault.VaultFile = dlgOpenVault.FileName) then
    begin
      FVault := Vault;
      FRoot := RootNode;
      CloseActiveVault;
      Application.ProcessMessages;
      Break;
    end;

    RootNode := RootNode.getNextSibling;
  end;

  VaultOK := true;
  Vault := TcbvCBVault.Create(nil);
  Vault.OnProgress := OnVaultProgress;
  Vault.VaultFile := dlgOpenVault.FileName;
  Vault.ReadOnly := ofReadOnly in dlgOpenVault.Options;
  //if Vault.GetEncryption <> VAULT_EM_NONE then
  //  begin
  //    SolFSVaultPasswordNeeded(Vault, dlgOpenVault.VaultFile, 0, );
  //  end;
  try
    Vault.CheckAndRepair(0);
  except
    on E: Exception do
    begin
      VaultOK := false;
      Application.ShowException(E);
    end;
  end;

  FreeAndNil(Vault);

  if VaultOK then
  begin
    MessageDlg(VaultCheckAndRepairOK, mtInformation, [mbOK], 0);
    OpenVault(dlgOpenVault.FileName, '', ofReadOnly in dlgOpenVault.Options);
  end;
end;

procedure TFormVaultExplorer.actCloseAllVaultsExecute(Sender: TObject);
begin
  CloseAllVaults;
end;

procedure TFormVaultExplorer.actCloseAllVaultsUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (tvFolders.Items.Count <> 0);
end;

procedure TFormVaultExplorer.actCloseVaultExecute(Sender: TObject);
begin
  CloseActiveVault;
end;

procedure TFormVaultExplorer.actCloseVaultUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FVault);
end;

procedure TFormVaultExplorer.actCompactExecute(Sender: TObject);
var
  DiskSize: Int64;
begin
  if Assigned(FVault) then
  begin
    ClearBuffer;
    DiskSize := FVault.VaultSize;
    FVault.CompactVault();
    MessageDlg(Format(VaultCompacted, [FileSizeToStr(DiskSize -
      FVault.VaultSize)]), mtInformation, [mbOK], 0);
  end;
end;

procedure TFormVaultExplorer.actCompactUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FVault) and
    FVault.Active and not FVault.ReadOnly;
end;

procedure TFormVaultExplorer.actCopyExecute(Sender: TObject);
var
  i, k, n: Integer;
begin
  ClearBuffer;
  BufferCutFiles := false;
  if not Assigned(FVault) or not FVault.Active then
    Exit;

  if tvFolders.Focused and (tvFolders.Selected <> nil) and
     (FRoot <> tvFolders.Selected) then
  begin
    BufferVault := FVault;
    SetLength(BufferFiles, 1);
    BufferFiles[0] := GetNodePath(tvFolders.Selected);
    Exit;
  end;

  if lvFiles.Focused then
  begin
    BufferVault := FVault;
    n := 0;
    for i := 0 to lvFiles.Items.Count - 1 do
      if lvFiles.Items[i].Selected then
        Inc(n);

    SetLength(BufferFiles, n);
    k := 0;
    for i := 0 to lvFiles.Items.Count - 1 do
      if lvFiles.Items[i].Selected then
      begin
        BufferFiles[k] := FPath + chr(FVault.PathSeparator) + lvFiles.Items[i].Caption;
        Inc(k);
      end;
  end;
end;

procedure TFormVaultExplorer.actCopyUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FVault) and FVault.Active and
    ((lvFiles.Focused and (lvFiles.Selected <> nil)) or
     (tvFolders.Focused and (tvFolders.Selected <> nil) and
      (FRoot <> tvFolders.Selected)));
end;

procedure TFormVaultExplorer.actCopyToFolderExecute(Sender: TObject);
var
  Node: TTreeNode;
  FileName: String;
  i, n: Integer;
  FromFolders: Boolean;
begin
  if not Assigned(FVault) or
     not FVault.Active then
    Exit;

  ClearBuffer;
  if tvFolders.Focused and (tvFolders.Selected <> nil) and (FRoot <> FNode) then
  begin
    FromFolders := true;
    n := 1;
    FileName := ExtractFileNameEx(GetNodePath(tvFolders.Selected), chr(FVault.PathSeparator));
  end
  else
    if lvFiles.Focused and (lvFiles.Selected <> nil) then
    begin
      FromFolders := false;
      n := 0;
      FileName := '';
      for i := 0 to lvFiles.Items.Count - 1 do
        if lvFiles.Items[i].Selected then
        begin
          if n = 0 then
            FileName := lvFiles.Items[i].Caption;

          Inc(n);
        end;
    end
    else
      Exit;

  with TFormVaultselectfolder.Create(Self) do
    try
      Node := Self.tvFolders.Items.GetFirstNode;
      while Node <> nil do
      begin
        AddVault(GetNodeVault(Node));
        Node := Node.getNextSibling;
      end;

      Caption := CopyToFolder_Caption;
      btnOK.Caption := CopyToFolder_ButtonCaption;
      if n = 1 then
        Title := Format(CopyToFolder_Title, [FileName])
      else
        Title := Format(CopyToFolder_MultipleTitle, [n]);

      Vault := FVault;
      Folder := FPath;
      if ShowModal = mrOK then
      begin
        if FromFolders then
          CopyFiles(FVault, Vault, GetNodePath(Self.tvFolders.Selected), Folder)
        else // lvFiles.Focused
          for i := 0 to lvFiles.Items.Count - 1 do
            if lvFiles.Items[i].Selected then
              CopyFiles(FVault, Vault, FPath + chr(FVault.PathSeparator) + lvFiles.Items[i].Caption, Folder);

        ClearFileOptions(Vault);
        Node := Self.tvFolders.Items.GetFirstNode;
        while Node <> nil do
        begin
          if GetNodeVault(Node) = Vault then
          begin
            UpdateNodes(Node);
            Break;
          end;

          Node := Node.getNextSibling;
        end;
      end;
    finally
      Free;
    end;
end;

procedure TFormVaultExplorer.actCopyToFolderUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FVault) and FVault.Active and
     ((lvFiles.Focused and (lvFiles.Selected <> nil)) or
      (tvFolders.Focused and (tvFolders.Selected <> nil) and (FRoot <> FNode)));
end;

procedure TFormVaultExplorer.actCutExecute(Sender: TObject);
begin
  actCopyExecute(Sender);
  BufferCutFiles := true;
end;

procedure TFormVaultExplorer.actCutUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FVault) and
     FVault.Active and not FVault.ReadOnly and
     ((lvFiles.Focused and (lvFiles.Selected <> nil)) or
      (tvFolders.Focused and (tvFolders.Selected <> nil) and
       (FRoot <> tvFolders.Selected)));
end;

procedure TFormVaultExplorer.actDeleteExecute(Sender: TObject);
var
  i, n: Integer;
  ParentNode: TTreeNode;
  FullName: String;
begin
  ClearBuffer;
  DeleteConfirmationAsked := false;
  if tvFolders.Focused and (tvFolders.Selected <> nil) then
  begin
    ParentNode := tvFolders.Selected.Parent;
    FullName := GetNodePath(tvFolders.Selected);
    DeleteFiles(FVault, FullName, VAULT_FATTR_DIRECTORY);
  end
  else if lvFiles.Focused and (lvFiles.Selected <> nil) then
  begin
    n := 0;
    for i := 0 to lvFiles.Items.Count - 1 do
      if lvFiles.Items[i].Selected then
        Inc(n);

    if n > 1 then
    begin
      if MessageDlg(Format(MultipleFileDeleteConfirmation, [n]),
          mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        DeleteConfirmationAsked := true
      else
        Exit;
    end;

    ParentNode := FNode;
    for i := 0 to lvFiles.Items.Count - 1 do
      if lvFiles.Items[i].Selected then
      begin
        FullName := FPath + chr(FVault.PathSeparator) + lvFiles.Items[i].Caption;
        DeleteFiles(FVault, FullName, PListFileInfo(lvFiles.Items[i].Data).Attributes);
      end;
  end
  else
    Exit;

  UpdateFolder(ParentNode);
  UpdateFiles(ParentNode);
  FRoot.Expand(False);
end;

procedure TFormVaultExplorer.actDeleteUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FVault) and
    FVault.Active and not FVault.ReadOnly and
    (tvFolders.Focused and ((tvFolders.Selected <> nil) and
    (tvFolders.Selected <> FRoot)) or
    (lvFiles.Focused and (lvFiles.Selected <> nil)));
end;

procedure TFormVaultExplorer.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TFormVaultExplorer.actExportByMaskExecute(Sender: TObject);
var
Flags :Integer;
begin
  Application.CreateForm(TFormExport, FormExport);
  try
    if FormExport.ShowModal = mrOK then
      begin
        Flags := VAULT_CFF_OVERWRITE_ALL;
        if FormExport.chbRecursive.Checked then Flags := Flags or VAULT_CFF_INCLUDE_SUBDIRS_WITH_CONTENTS;
        FVault.CopyFromVault(FPath, FormExport.edtDiskPath.Text, FormExport.edtMask.Text,Flags,'');
        UpdateFolder(FNode);
      end;
  finally
    FreeAndNil(FormExport);
  end;
end;

procedure TFormVaultExplorer.actExportByMaskUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FVault) and
    FVault.Active;
end;

procedure TFormVaultExplorer.actExportFileExecute(Sender: TObject);
var
  Stop: Boolean;
  FileName: String;
  DiskStream: TFileStream;
  VaultStream: TCBFSVaultStream;
  Total, Done: Int64;
  BufSize, Size, Divider: Integer;
  Buffer: PChar;
begin
  FileName := FPath + chr(FVault.PathSeparator) + lvFiles.Selected.Caption;
  dlgSaveFile.FileName := lvFiles.Selected.Caption;
  if dlgSaveFile.Execute then
  begin
    VaultStream := FVault.OpenFile(FileName, VAULT_OM_OPEN_EXISTING, true, false, '');
    DiskStream := TFileStream.Create(dlgSaveFile.FileName, fmCreate);
    BufSize := 16 * 1024;
    Buffer := AllocMem(BufSize);
    try
      Done := 0;
      Total := VaultStream.Size;
      Divider := 1;
      while Total shr Divider > MaxInt do
        Inc(Divider);

      Stop := false;
      while true do
      begin
        Size := VaultStream.Read(Buffer^, BufSize);
        if Size > 0 then
          DiskStream.WriteBuffer(Buffer^, Size);
        FlushFileBuffers(DiskStream.Handle);

        Inc(Done, Size);
        OnVaultProgress(FVault, VAULT_PO_COPYING_FILES_TO_VAULT, FileName,
          Done shr Divider, Total shr Divider, true, Stop);
        if Stop or (Size < BufSize) then
          Break;
      end;
    finally
      VaultStream.Free;
      DiskStream.Free;
      FreeMem(Buffer);
    end;
  end;
end;

procedure TFormVaultExplorer.actExportFileUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FVault) and FVault.Active and
    (lvFiles.Focused and (lvFiles.Selected <> nil) and
    ((PListFileInfo(lvFiles.Selected.Data).Attributes and VAULT_FATTR_DIRECTORY) = 0));
end;

procedure TFormVaultExplorer.actImportByMaskExecute(Sender: TObject);
var
Flags :Integer;
begin
  with TFormImport.Create(Self) do
    try
      EncryptionChecked := (FVault.DefaultFileEncryption <> VAULT_EM_NONE);
      cbEncryption.Checked := (FVault.DefaultFileEncryption <> VAULT_EM_NONE);
      cbCompress.Checked := (FVault.DefaultFileCompression <> VAULT_CM_NONE);
      tbCompressionLevel.Position := 0;
      if ShowModal = mrOK then
      begin
        if cbEncryption.Checked then
        begin
          FVault.DefaultFileEncryption := VAULT_EM_DEFAULT;
          FVault.DefaultFileCreatePassword := edPassword.Text;
        end
        else
        begin
          FVault.DefaultFileEncryption := VAULT_EM_NONE;
          FVault.DefaultFileCreatePassword := '';
        end;

        if cbCompress.Checked then
        begin
          if tbCompressionLevel.Position = 0 then
          begin
            FVault.DefaultFileCompression := VAULT_CM_DEFAULT;
            //FVault.DefaultFileCompressionLevel := 0;
          end
          else
          begin
            FVault.DefaultFileCompression := VAULT_CM_ZLIB;
            //FVault.DefaultFileCompressionLevel := tbCompressionLevel.Position;
          end;
        end
        else
        begin
          FVault.DefaultFileCompression := VAULT_CM_NONE;
          //FVault.DefaultFileCompressionLevel := 0;
        end;
        Flags := VAULT_CFF_OVERWRITE_ALL;
        if cbRecursive.Checked then Flags := Flags or VAULT_CFF_INCLUDE_SUBDIRS_WITH_CONTENTS;
        FVault.CopyToVault(edDiskPath.Text, FPath, edMask.Text, Flags, VAULT_EM_NONE, '', VAULT_CM_NONE, 0, 0);

        UpdateFolder(FNode);
      end;
    finally
      Free;
    end;
end;

procedure TFormVaultExplorer.actImportByMaskUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FVault) and
    FVault.Active and not FVault.ReadOnly;
end;

procedure TFormVaultExplorer.actImportFileExecute(Sender: TObject);
var
  FileName: String;
  Dummy: Boolean;
begin
  if not Assigned(FVault) or
     not FVault.Active then
    Exit;

  dlgOpenFile.FileName := '';
  if dlgOpenFile.Execute then
  begin
    FileName := FPath + chr(FVault.PathSeparator) +
      ExtractFileName(dlgOpenFile.FileName);

    Dummy := false;
    if SetFilesEncryptionAndCompression(FVault, FileName, Dummy) then
      FileToVault(dlgOpenFile.FileName, FileName);

    UpdateFiles(FNode);
  end;

  ClearFileOptions(FVault);
end;

procedure TFormVaultExplorer.actImportFileUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FVault) and
    FVault.Active and not FVault.ReadOnly;
end;

procedure TFormVaultExplorer.actInvertSelectionExecute(Sender: TObject);
var
  i: Integer;
begin
  lvFiles.SetFocus;
  for i := 0 to lvFiles.Items.Count - 1 do
    lvFiles.Items[i].Selected := not lvFiles.Items[i].Selected; 
end;

procedure TFormVaultExplorer.actInvertSelectionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := lvFiles.Focused and (lvFiles.Items.Count > 0);
end;

procedure TFormVaultExplorer.actMoveToFolderExecute(Sender: TObject);
var
  Node: TTreeNode;
  FileName: String;
  i, n: Integer;
  FromFolders: Boolean;
begin
  if not Assigned(FVault) or
     not FVault.Active then
    Exit;

  ClearBuffer;
  if tvFolders.Focused and (tvFolders.Selected <> nil) and (FRoot <> FNode) then
  begin
    FromFolders := true;
    n := 1;
    FileName := ExtractFileNameEx(GetNodePath(tvFolders.Selected), chr(FVault.PathSeparator));
  end
  else
    if lvFiles.Focused and (lvFiles.Selected <> nil) then
    begin
      FromFolders := false;
      n := 0;
      FileName := '';
      for i := 0 to lvFiles.Items.Count - 1 do
        if lvFiles.Items[i].Selected then
        begin
          if n = 0 then
            FileName := lvFiles.Items[i].Caption;

          Inc(n);
        end;
    end
    else
      Exit;

  with TFormVaultselectfolder.Create(Self) do
    try
      Node := Self.tvFolders.Items.GetFirstNode;
      while Node <> nil do
      begin
        AddVault(GetNodeVault(Node));
        Node := Node.getNextSibling;
      end;

      Caption := MoveToFolder_Caption;
      btnOK.Caption := MoveToFolder_ButtonCaption;
      if n = 1 then
        Title := Format(MoveToFolder_Title, [FileName])
      else
        Title := Format(MoveToFolder_MultipleTitle, [n]);

      Vault := FVault;
      Folder := FPath;
      if ShowModal = mrOK then
      begin
        if FromFolders then
        begin
          MoveFiles(FVault, Vault, GetNodePath(Self.tvFolders.Selected), Folder);
          UpdateNodes(Self.tvFolders.Selected.Parent);
        end
        else // lvFiles.Focused
        begin
          for i := 0 to lvFiles.Items.Count - 1 do
            if lvFiles.Items[i].Selected then
              MoveFiles(FVault, Vault, FPath + chr(FVault.PathSeparator) + lvFiles.Items[i].Caption, Folder);

          UpdateNodes(Self.FNode);
        end;

        ClearFileOptions(Vault);
        Node := Self.tvFolders.Items.GetFirstNode;
        while Node <> nil do
        begin
          if GetNodeVault(Node) = Vault then
          begin
            UpdateNodes(Node);
            Self.FRoot := Node;
            SelectNode(Folder);
            Break;
          end;

          Node := Node.getNextSibling;
        end;
      end;
    finally
      Free;
    end;
end;

procedure TFormVaultExplorer.actMoveToFolderUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FVault) and
     FVault.Active and not FVault.ReadOnly and
     ((lvFiles.Focused and (lvFiles.Selected <> nil)) or
      (tvFolders.Focused and (tvFolders.Selected <> nil) and (FRoot <> FNode)));
end;

procedure TFormVaultExplorer.actNewFolderExecute(Sender: TObject);
var
  i: Integer;
  Item: TListItem;
  BaseName, FullName, FileName: String;
begin
  ClearBuffer;
  i := 0;
  BaseName := 'New Folder';
  while true do
    begin
      if i > 0 then
        FileName := BaseName + ' ' + IntToStr(i)
      else
        FileName := BaseName;

      FullName := FPath + chr(FVault.PathSeparator) + FileName;
      if not FVault.FileExists(FullName) then
        Break;

      Inc(i);
    end;

  try
    FVault.CreateDirectory(FullName, true);
  except
    on E: Exception do
      Application.ShowException(E);
  end;

  UpdateFolder(FNode);
  Item := nil;
  for i := 0 to lvFiles.Items.Count - 1 do
    if lvFiles.Items[i].Caption = FileName then
      begin
        Item := lvFiles.Items[i];
        Break;
      end;

  if Item <> nil then
    Item.EditCaption;
end;

procedure TFormVaultExplorer.actNewFolderUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FVault) and
    FVault.Active and not FVault.ReadOnly;
end;

procedure TFormVaultExplorer.actNewLinkExecute(Sender: TObject);
var
  LinkPath, LinkName: String;
  i: Integer;
begin
  ClearBuffer;
  with TFormLink.Create(Self) do
    try
      Vault := FVault;

      if tvFolders.Focused then
        LinkPath := GetNodePath(tvFolders.Selected)
      else
        LinkPath := FPath;

      if lvFiles.Focused and (lvFiles.Selected <> nil) then
      begin
        edDestPath.Text := FPath + chr(FVault.PathSeparator) + lvFiles.Selected.Caption;
        LinkName := ChangeFileExt(lvFiles.Selected.Caption, '');
      end
      else
      begin
        edDestPath.Text := '';
        LinkName := 'New Link';
      end;

      if FVault.FileExists(LinkPath + chr(FVault.PathSeparator) + LinkName) then
      begin
        i := 1;
        while FVault.FileExists(LinkPath + chr(FVault.PathSeparator) + LinkName + ' ' + IntToStr(i)) do
          Inc(i);

        LinkName := LinkName + ' ' + IntToStr(i);
      end;

      edLinkName.Text := LinkPath + chr(FVault.PathSeparator) + LinkName;

      if ShowModal = mrOK then
      begin
        try
          FVault.CreateLink(edLinkName.Text, edDestPath.Text);
        except
          on E: Exception do
            Application.ShowException(E);
        end;
        UpdateFolder(FNode);
        //UpdateFiles(FNode);
      end;
    finally
      Free;
    end;
end;

procedure TFormVaultExplorer.actNewLinkUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FVault) and
    FVault.Active and not FVault.ReadOnly;
end;

procedure TFormVaultExplorer.actOpenExecute(Sender: TObject);
var
  i: Integer;
begin
  if not Assigned(FVault) or not FVault.Active or
     not lvFiles.Focused or (lvFiles.Selected = nil) then
    Exit;

  if actExportFile.Enabled then
    actExportFile.Execute
  else
    begin
      if (PListFileInfo(lvFiles.Selected.Data).Attributes and VAULT_FATTR_DIRECTORY) <> 0 then
        begin
          if FNode.HasChildren and (FNode.Count = 0) then
            ExpandFolder(FNode);

          for i := 0 to FNode.Count - 1 do
            if FNode.Item[i].Text = lvFiles.Selected.Caption then
            begin
              FNode.Expand(False);
              FNode.Item[i].Selected := true;
              Break;
            end;
        end;
    end;
end;

procedure TFormVaultExplorer.actOpenUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FVault) and
    FVault.Active and
    (lvFiles.Focused and (lvFiles.Selected <> nil));
end;

procedure TFormVaultExplorer.actOpenVaultExecute(Sender: TObject);
var
  AutoCompact: LongWord;
begin
  with TFormOpenvault.Create(Self) do
    try
      if ShowModal = mrOK then
      begin
        AutoCompact := LongWord(spnAutoCompact.Position);
        if not cbAutoCompact.Checked then
          AutoCompact := 0;

        OpenVault(edVaultFileName.Text, '',
          cbReadOnly.Checked, cbUseJournaling.Checked,
          cbUseAccessTime.Checked, AutoCompact);
      end;
    finally
      Free;
    end;
end;

procedure TFormVaultExplorer.actPasteExecute(Sender: TObject);
var
  i: Integer;
  RootNode: TTreeNode;
begin
  if not Assigned(FVault) or not FVault.Active or
     FVault.ReadOnly then
    Exit;

  CopyFileOptionsSeted := false;
  MoveFileOptionsSeted := false;
  for i := 0 to Length(BufferFiles) - 1 do
  begin
    if BufferCutFiles then
      MoveFiles(BufferVault, FVault, BufferFiles[i], FPath)
    else
      CopyFiles(BufferVault, FVault, BufferFiles[i], FPath);
  end;

  RootNode := tvFolders.Items.GetFirstNode;
  while RootNode <> nil do
  begin
    if GetNodeVault(RootNode) = BufferVault then
    begin
      UpdateNodes(RootNode);
      Break;
    end;

    RootNode := RootNode.getNextSibling;
  end;

  if BufferVault <> FVault then
    UpdateFolder(FNode)
  else
    SelectNode(FPath);

  if BufferCutFiles then
    ClearBuffer;
end;

procedure TFormVaultExplorer.actPasteUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FVault) and
    FVault.Active and not FVault.ReadOnly and
    (Length(BufferFiles) <> 0);
end;

procedure TFormVaultExplorer.actPropertiesExecute(Sender: TObject);
var
  I: Integer;
  FullName, FilePath, FileName, Password, DestinationName: String;
  OldAttributes, NewAttributes: Integer;
  Encryption: Integer;
  Compression: Integer;
  Changed, Cancel: Boolean;
begin
  Changed := false;
  if tvFolders.Focused and (tvFolders.Selected <> nil) then
    begin
      if tvFolders.Selected = FRoot then
        begin
          actVaultPropsExecute(nil);
          Exit;
        end
      else
        begin
          I := tvFolders.Selected.ImageIndex;
          FileName := tvFolders.Selected.Text;
          FilePath := GetNodePath(tvFolders.Selected.Parent);
        end;
    end
  else
    begin
      I := lvFiles.Selected.ImageIndex;
      FileName := lvFiles.Selected.Caption;
      FilePath := FPath;
    end;

  FullName := FilePath + chr(FVault.PathSeparator) + FileName;
  if FVault.FileExists(FullName) then
  begin
    Application.CreateForm(TFormFileprops, FormFileprops);
    try
      FormFileprops.Vault := FVault;
      FormFileprops.FileName := FullName;
      OldAttributes := FormFileprops.Attributes;

      iglFilesLarge.GetIcon(I, FormFileprops.imgIcon.Picture.Icon);

      Compression := FVault.GetFileCompression(FullName);
      FormFileprops.CompressionLevel := 0;
      Encryption := FVault.GetFileEncryption(FullName);
      if ((OldAttributes and VAULT_FATTR_ENCRYPTED) <> 0) and
         (Encryption = VAULT_EM_NONE) then
        FormFileprops.Encryption := VAULT_EM_DEFAULT
      else
        FormFileprops.Encryption := Encryption;

      if FormFileprops.ShowModal = mrOK then
      begin
        NewAttributes := FormFileprops.Attributes;
        Password := '';

        if (((NewAttributes xor OldAttributes) and VAULT_FATTR_COMPRESSED) <> 0) or
           (Compression <> FormFileprops.Compression) then
        begin
          i := 0;
          try
            Cancel := false;
            if Encryption <> VAULT_EM_NONE then
            begin
              repeat
                try
                  if FVault.CheckFilePassword(FullName, Password) then
                    Break;
                except
                  on E: EcbvCBVault do
                  begin
                    if E.Code <> VAULT_ERR_INVALID_PASSWORD then
                      raise;
                  end
                  else
                    raise
                end;

                Inc(i);
              until Cancel;
            end;

            if not Cancel then
            begin
              FVault.SetFileCompression(FullName, FormFileprops.Compression,
                FormFileprops.CompressionLevel, 0, Password);
              Changed := true;
            end;
          except
            on E: Exception do
              Application.ShowException(E);
          end;

          OldAttributes := OldAttributes xor VAULT_FATTR_COMPRESSED;
        end;

        if (((NewAttributes xor OldAttributes) and VAULT_FATTR_ENCRYPTED) <> 0) or
           ((Encryption <> FormFileprops.Encryption) and
            (((OldAttributes and VAULT_FATTR_ENCRYPTED) = 0) or (Encryption <> VAULT_EM_NONE))) or
           (FormFileprops.Password <> '') then
        begin
          i := 0;
          try
            Cancel := false;
            if Encryption <> VAULT_EM_NONE then
            begin
              repeat
                try
                  if FVault.CheckFilePassword(FullName, Password) then
                    Break;
                except
                  on E: EcbvCBVault do
                  begin
                    if E.Code <> VAULT_ERR_INVALID_PASSWORD then
                      raise;
                  end
                  else
                    raise
                end;

                Inc(i);
              until Cancel;
            end;

            if not Cancel and
               ((FormFileprops.Encryption = VAULT_EM_NONE) or
                (Password <> FormFileprops.Password)) then
            begin
              FVault.SetFileEncryption(FullName, FormFileprops.Encryption,
                Password, FormFileprops.Password);
              Changed := true;
            end;
          except
            on E: Exception do
              Application.ShowException(E);
          end;

          OldAttributes := OldAttributes xor VAULT_FATTR_ENCRYPTED;
        end;

        if NewAttributes <> OldAttributes then
        begin
          FVault.SetFileAttributes(FullName, NewAttributes);
          Changed := true;
        end;

        if FormFileprops.edtFileName.Text <> FileName then
        begin
          FVault.MoveFile(FullName, FPath + chr(FVault.PathSeparator) +
            FormFileprops.edtFileName.Text, false);
          Changed := true;
        end;

        if (OldAttributes and VAULT_FATTR_SYMLINK) <> 0 then
        begin
          try
            DestinationName := FVault.ResolveLink(FullName, true);
            if DestinationName <> FormFileprops.edTarget.Text then
            begin
              Changed := true;
              FVault.DeleteFile(FullName);

              FVault.CreateLink(FullName, FormFileprops.edTarget.Text);
              if tvFolders.Focused then
                FNode := FNode.Parent;
            end;
          except
            on E: Exception do
              Application.ShowException(E);
          end;
        end;

        if Changed then
          Refresh;
      end;
    finally
      FreeAndNil(FormFileprops);
    end;
  end;
end;

procedure TFormVaultExplorer.actPropertiesUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FVault) and
    FVault.Active and
    (tvFolders.Focused and ((tvFolders.Selected <> nil)) or
    (lvFiles.Focused and (lvFiles.Selected <> nil)));
end;

procedure TFormVaultExplorer.actRefreshExecute(Sender: TObject);
begin
  Refresh;
end;

procedure TFormVaultExplorer.actRefreshUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FVault) and FVault.Active;
end;

procedure TFormVaultExplorer.actRenameExecute(Sender: TObject);
begin
  if tvFolders.Focused and (tvFolders.Selected <> nil) then
    tvFolders.Selected.EditText
  else if lvFiles.Focused and (lvFiles.Selected <> nil) then
    lvFiles.Selected.EditCaption
  else
    Exit;
end;

procedure TFormVaultExplorer.actSelectAllExecute(Sender: TObject);
var
  i: Integer;
begin
  lvFiles.SetFocus;
  for i := 0 to lvFiles.Items.Count - 1 do
    lvFiles.Items[i].Selected := true;
end;

procedure TFormVaultExplorer.actSelectAllUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := lvFiles.Focused and (lvFiles.Items.Count > 0);
end;

procedure TFormVaultExplorer.actUpExecute(Sender: TObject);
begin
  if FNode.Parent <> nil then
    FNode.Parent.Selected := true;
end;

procedure TFormVaultExplorer.actUpUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FVault) and
    FVault.Active and (FNode <> nil) and (FNode.Parent <> nil);
end;

procedure TFormVaultExplorer.actViewModeExecute(Sender: TObject);
begin
  lvFiles.ViewStyle := TViewStyle((Sender as TAction).Tag);
end;

procedure TFormVaultExplorer.actViewModeUpdate(Sender: TObject);
var
  Action: TAction;
begin
  Action := Sender as TAction;
  Action.Enabled := Assigned(FVault) and FVault.Active;
  Action.Checked := Integer(lvFiles.ViewStyle) = Action.Tag;
end;

procedure TFormVaultExplorer.appEventsException(Sender: TObject;
  E: Exception);
begin
  Application.ShowException(E);
  if FormProgress.Visible then
  begin
    FormProgress.Hide;
    Enabled := true;
  end;
end;

procedure TFormVaultExplorer.ClearBuffer;
begin
  BufferFiles := nil;
  BufferVault := nil;
end;

procedure TFormVaultExplorer.ClearFileOptions(Vault: TcbvCBVault);
begin
  Vault.DefaultFileCreatePassword := '';
end;

procedure TFormVaultExplorer.CloseActiveVault;
begin
  if Assigned(FVault) and
     Assigned(FRoot) then
  begin
    tvFolders.Selected := nil;
    FRoot.Data := nil;
    FreeAndNil(FVault);
    FRoot.Delete;
    FPath := '';
    FRoot := nil;
    FNode := nil;
    ClearBuffer;
    lvFiles.Items.BeginUpdate;
    lvFiles.Items.Clear;
    lvFiles.Items.EndUpdate;
  end;
end;

procedure TFormVaultExplorer.CloseAllVaults;
var
  i: Integer;
  Node: TTreeNode;
begin
  FVault := nil;
  FPath := '';
  FRoot := nil;
  FNode := nil;
  ClearBuffer;

  i := 0;
  tvFolders.Items.BeginUpdate;
  try
    while i < tvFolders.Items.Count do
    begin
      if tvFolders.Items[i].Parent = nil then
      begin
        Node := tvFolders.Items[i];
        TcbvCBVault(Node.Data).Free;
        Node.Data := nil;
        Node.Delete;
        i := 0;
      end;
    end;
  finally
    tvFolders.Items.EndUpdate;
  end;

  lvFiles.Items.BeginUpdate;
  lvFiles.Items.Clear;
  lvFiles.Items.EndUpdate;
end;

procedure TFormVaultExplorer.FileToVault(const FileName, VaultFileName: String);
var
  Stop: Boolean;
  DiskStream: TFileStream;
  VaultStream: TCBFSVaultStream;
  Total, Done: Int64;
  BufSize, Size, Divider: Integer;
  Buffer: PChar;
begin
  if not Assigned(FVault) or
     not FVault.Active then
    Exit;

  DiskStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    VaultStream := FVault.OpenFile(VaultFileName, VAULT_OM_CREATE_ALWAYS, false, true, '');
    try
      try
        VaultStream.Size := DiskStream.Size;
        Done := 0;
        Total := GetFileSize(DiskStream.Handle, @BufSize);
        Total := Total or (Int64(BufSize) shl 32);
        Divider := 1;
        while Total shr Divider > MaxInt do
          Inc(Divider);

        BufSize := 16 * 1024;
        Buffer := AllocMem(BufSize);
        try
          Stop := false;
          while true do
          begin
            Size := DiskStream.Read(Buffer^, BufSize);
            if Size > 0 then
              VaultStream.WriteBuffer(Buffer^, Size);

            Inc(Done, Size);
            OnVaultProgress(FVault, VAULT_PO_COPYING_FILES_TO_VAULT, FileName,
            Done shr Divider, Total shr Divider, true, Stop);
            if Stop or (Size < BufSize) then
              Break;
          end;
        finally
          FreeMem(Buffer);
        end;
      finally
        VaultStream.Free;
      end;
    except
      try
        FVault.DeleteFile(VaultFileName);
      except
      end;
      raise;
    end;
  finally
    DiskStream.Free;
  end;
end;

procedure TFormVaultExplorer.CopyFileToVault(SourceVault,
  DestVault: TcbvCBVault; const SourceFileName, DestFileName: String);
var
  Stop: Boolean;
  SourceStream, DestStream: TCBFSVaultStream;
  Total, Done: Int64;
  BufSize, Size, Divider: Integer;
  Buffer: PChar;
begin
  if not Assigned(SourceVault) or not SourceVault.Active or
     not Assigned(DestVault) or not DestVault.Active then
    Exit;

  SourceStream := SourceVault.OpenFile(SourceFileName, VAULT_OM_OPEN_EXISTING, true, false, '');
  try
    DestStream := DestVault.OpenFile(DestFileName, VAULT_OM_CREATE_ALWAYS, false, true, '');
    try
      try
        Done := 0;
        Total := SourceStream.Size;
        Divider := 1;
        while Total shr Divider > MaxInt do
          Inc(Divider);

        BufSize := 16 * 1024;
        Buffer := AllocMem(BufSize);
        try
          Stop := false;
          while true do
          begin
            Size := SourceStream.Read(Buffer^, BufSize);
            if Size > 0 then
              DestStream.WriteBuffer(Buffer^, Size);

            Inc(Done, Size);
            OnVaultProgress(DestVault, VAULT_PO_COPYING_FILES_TO_VAULT, SourceFileName,
                Done shr Divider, Total shr Divider, true, Stop);
            if Stop or (Size < BufSize) then
              Break;
          end;
        finally
          FreeMem(Buffer);
        end;
      finally
        DestStream.Free;
      end;
    except
      DestVault.DeleteFile(DestFileName);
      raise;
    end;
  finally
    SourceStream.Free;
  end;
end;

function TFormVaultExplorer.SetFilesEncryptionAndCompression(Vault: TcbvCBVault;
  const FileName: String; var ApplyToAllFiles: Boolean): Boolean;
var
  Res: Integer;
begin
  if not Assigned(Vault) or
     not Vault.Active then
  begin
    Result := false;
    Exit;
  end;

  Result := true;
  with TFormImportfile.Create(Self) do
    try
      lbText.Caption := Format(IMPORT_FILE_SelectMessage, [string(FileName)]);
      EncryptionChecked := (Vault.DefaultFileEncryption <> VAULT_EM_NONE);
      cbEncryption.Checked := (Vault.DefaultFileEncryption <> VAULT_EM_NONE);
      cbCompress.Checked := (Vault.DefaultFileCompression <> VAULT_CM_NONE);
      tbCompressionLevel.Position := 0;
      btnAll.Visible := ApplyToAllFiles;
      Res := ShowModal;
      if (Res = mrOK) or (Res = mrYesToAll) then
      begin
        ApplyToAllFiles := (Res = mrYesToAll);
        if cbEncryption.Checked then
        begin
          Vault.DefaultFileEncryption := VAULT_EM_DEFAULT;
          Vault.DefaultFileCreatePassword := edPassword.Text;
        end
        else
        begin
          Vault.DefaultFileEncryption := VAULT_EM_NONE;
          Vault.DefaultFileCreatePassword := '';
        end;

        if cbCompress.Checked then
        begin
          Res := tbCompressionLevel.Position;
          if Res = 0 then
          begin
            Vault.DefaultFileCompression := VAULT_CM_DEFAULT;
            //Vault.DefaultFileCompressionLevel := 0;
          end
          else
          begin
            Vault.DefaultFileCompression := VAULT_CM_ZLIB;
            //Vault.DefaultFileCompressionLevel := Res;
          end;
        end
        else
        begin
          Vault.DefaultFileCompression := VAULT_CM_NONE;
          //Vault.DefaultFileCompressionLevel := 0;
        end;
      end
      else
        Result := false;
    finally
      Free;
    end;
end;

procedure TFormVaultExplorer.DeleteFiles(Vault: TcbvCBVault; const FileName: String; Attributes: LongWord);

  procedure InternalDeleteFolder(const FolderName: String); forward;

  procedure InternalDelete(const FileName: String; Attributes: LongWord);
  var
    Stream: TCBFSVaultStream;
  begin
    try
      if (Attributes and VAULT_FATTR_DIRECTORY) <> 0 then
      begin
        if DeleteConfirmationAsked or
           (MessageDlg(Format(FolderDeleteConfirmation, [ExtractFileNameEx(FileName, chr(Vault.PathSeparator))]),
              mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
        begin
          DeleteConfirmationAsked := true;
          InternalDeleteFolder(FileName);
        end;
      end
      else
      begin
        if DeleteConfirmationAsked or
           (MessageDlg(Format(FileDeleteConfirmation, [ExtractFileNameEx(FileName, chr(Vault.PathSeparator))]),
              mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
        begin
          DeleteConfirmationAsked := true;
          if (Attributes and VAULT_FATTR_SYMLINK) = 0 then
          begin
            // if file password protected, then password will be asked
            Stream := Vault.OpenFile(FileName, VAULT_OM_OPEN_EXISTING, true, false, '');
            try
//              Stream.Size := 0;
            finally
              Stream.Free;
            end;
          end;

          Vault.DeleteFile(FileName);
        end;
      end;
    except
      on E: Exception do
        Application.ShowException(E);
    end;
  end;

  procedure InternalDeleteFolder(const FolderName: String);
  var
    Search: Int64;
    SearchResult: Boolean;
  begin
    Search := Vault.FindFirst(FolderName + chr(Vault.PathSeparator) + '*', VAULT_FATTR_ANY_FILE, 0);
    if Search <> -1 then
      try
        SearchResult := true;
        while SearchResult do
        begin
          InternalDelete(Vault.GetSearchResultFullName(Search), Vault.GetSearchResultAttributes(Search));
          SearchResult := Vault.FindNext(Search)
        end;
      finally
        Vault.FindClose(Search);
      end;

    try
      Vault.DeleteFile(FolderName);
    except
      on E: Exception do
        Application.ShowException(E);
    end;
  end;

begin
  if not Assigned(Vault) then
    Exit;

  InternalDelete(FileName, Attributes);
end;

procedure TFormVaultExplorer.CopyFiles(SourceVault,
  DestVault: TcbvCBVault; const SourceFileName, NewPath: String);

  function InternalCopyFolder(const SourceFolderName, NewPath: String): Boolean; forward;

  function InternalCopyFile(const SourceFileName, NewPath: String): Boolean;
  var
    Attributes: LongWord;
    FileName: String;
    Search: Int64;
    SourceFileSize, FileSize: Int64;
    ApplyToAllFiles: Boolean;
  begin
    Result := true;
    Attributes := SourceVault.GetFileAttributes(SourceFileName);
    if ((Attributes and VAULT_FATTR_DIRECTORY) = 0) then
    begin
      FileName := NewPath + chr(DestVault.PathSeparator) + ExtractFileNameEx(SourceFileName, chr(SourceVault.PathSeparator));
      Search := DestVault.FindFirst(FileName, VAULT_FATTR_ANY_FILE, 0);
      if Search <> -1 then
      begin
        FileSize := DestVault.GetSearchResultSize(Search);
        Attributes := DestVault.GetSearchResultAttributes(Search);
        DestVault.FindClose(Search);
        SourceFileSize := 0;
        Search := SourceVault.FindFirst(SourceFileName, VAULT_FATTR_ANY_FILE, 0);
        if Search <> -1 then
        begin
          SourceFileSize := SourceVault.GetSearchResultSize(Search);
          SourceVault.FindClose(Search);
        end;

        if MessageDlg(Format(ReplaceExistingFile, [string(FileName),
              FileSizeToStr(FileSize), DateTimeToStr(DestVault.GetFileModificationTime(FileName)),
              FileSizeToStr(SourceFileSize), DateTimeToStr(SourceVault.GetFileModificationTime(SourceFileName))]),
             mtConfirmation, [mbYes, mbNo], 0) = mrNo then
          Exit;

        DeleteConfirmationAsked := true;
        DeleteFiles(DestVault, FileName, Attributes);
      end;

      if not CopyFileOptionsSeted then
      begin
        ApplyToAllFiles := true;
        if not SetFilesEncryptionAndCompression(DestVault, FileName, ApplyToAllFiles) then
        begin
          Result := false;
          Exit;
        end;

        if ApplyToAllFiles then
          CopyFileOptionsSeted := true;
      end;

      try
        CopyFileToVault(SourceVault, DestVault, SourceFileName, FileName);
      except
        on E: Exception do
          Application.ShowException(E);
      end;
    end
    else
    begin
      Result := InternalCopyFolder(SourceFileName, NewPath);
    end;
  end;

  function InternalCopyFolder(const SourceFolderName, NewPath: String): Boolean;
  var
    FolderName: String;
    Search: Int64;
    SearchResult: Boolean;
  begin
    Result := true;
    FolderName := NewPath + chr(DestVault.PathSeparator) + ExtractFileNameEx(SourceFolderName, chr(SourceVault.PathSeparator));
    DestVault.CreateDirectory(FolderName, true);

    Search := SourceVault.FindFirst(SourceFolderName + chr(SourceVault.PathSeparator) + '*', VAULT_FATTR_ANY_FILE, 0);
    if Search<> -1 then
      try
        SearchResult := true;
        while SearchResult do
        begin
          Result := InternalCopyFile(SourceVault.GetSearchResultFullName(Search), FolderName);
          if not Result then
            Exit;
            
          SearchResult := SourceVault.FindNext(Search)
        end;
      finally
        SourceVault.FindClose(Search);
      end;
  end;

begin
  InternalCopyFile(SourceFileName, NewPath);
end;

function TFormVaultExplorer.MoveFile(Vault: TcbvCBVault;
 const OldFileName, NewFileName: String): Boolean;
var
  Attributes: LongWord;
begin
  Result := false;
  if not Assigned(Vault) or
     not Vault.Active then
    Exit;

  Attributes := Vault.GetFileAttributes(OldFileName);
  if OldFileName = NewFileName then
  begin
    if (Attributes and VAULT_FATTR_DIRECTORY) <> 0 then
      MessageDlg(Format(CannotMoveFolderToSelf, [ExtractFileNameEx(OldFileName, chr(Vault.PathSeparator))]),
        mtError, [mbOK], 0)
    else
      MessageDlg(Format(CannotMoveFileToSelf, [ExtractFileNameEx(OldFileName, chr(Vault.PathSeparator))]),
        mtError, [mbOK], 0);

    Exit;
  end;

  if ((Attributes and VAULT_FATTR_DIRECTORY) <> 0) and (Length(NewFileName) > Length(OldFileName)) and
     (Copy(NewFileName, 1, Length(OldFileName) + 1) = OldFileName + chr(Vault.PathSeparator)) then
  begin
    MessageDlg(Format(CannotMoveFolderToSubfolder, [ExtractFileNameEx(OldFileName, chr(Vault.PathSeparator))]),
      mtError, [mbOK], 0);

    Exit;
  end;

  try
    Vault.MoveFile(OldFileName, NewFileName, false);
    Result := true;
  except
    on E: Exception do
      Application.ShowException(E);
  end;
end;

procedure TFormVaultExplorer.MoveFiles(SourceVault,
  DestVault: TcbvCBVault; const SourceFileName, NewPath: String);

  function InternalMoveFolder(const SourceFolderName, NewPath: String): Boolean; forward;

  function InternalMoveFile(const SourceFileName, NewPath: String): Boolean;
  var
    Attributes: LongWord;
    FileName: String;
    Search: Int64;
    SourceFileSize, FileSize: Int64;
    ApplyToAllFiles: Boolean;
  begin
    Result := true;
    Attributes := SourceVault.GetFileAttributes(SourceFileName);
    if ((Attributes and VAULT_FATTR_DIRECTORY) = 0) then
    begin
      FileName := NewPath + chr(DestVault.PathSeparator) + ExtractFileNameEx(SourceFileName, chr(SourceVault.PathSeparator));
      Search := DestVault.FindFirst(FileName, VAULT_FATTR_ANY_FILE, 0);
      if Search <> -1 then
      begin
        FileSize := SourceVault.GetSearchResultSize(Search);
        Attributes := SourceVault.GetSearchResultAttributes(Search);
        DestVault.FindClose(Search);
        SourceFileSize := 0;
        Search := SourceVault.FindFirst(SourceFileName, VAULT_FATTR_ANY_FILE, 0);
        if Search <> -1 then
        begin
          SourceFileSize := SourceVault.GetSearchResultSize(Search);
          SourceVault.FindClose(Search);
        end;

        if MessageDlg(Format(ReplaceExistingFile, [string(FileName),
              FileSizeToStr(FileSize), DateTimeToStr(DestVault.GetFileModificationTime(FileName)),
              FileSizeToStr(SourceFileSize), DateTimeToStr(SourceVault.GetFileModificationTime(SourceFileName))]),
             mtConfirmation, [mbYes, mbNo], 0) = mrNo then
          Exit;

        DeleteConfirmationAsked := true;
        DeleteFiles(DestVault, FileName, Attributes);
      end;

      if SourceVault <> DestVault then
      begin
        if not MoveFileOptionsSeted then
        begin
          ApplyToAllFiles := true;
          if not SetFilesEncryptionAndCompression(DestVault, FileName, ApplyToAllFiles) then
          begin
            Result := false;
            Exit;
          end;

          if ApplyToAllFiles then
            MoveFileOptionsSeted := true;
        end;

        try
          CopyFileToVault(SourceVault, DestVault, SourceFileName, FileName);
          SourceVault.DeleteFile(SourceFileName);
        except
          on E: Exception do
            Application.ShowException(E);
        end;
      end
      else
      begin
        try
          SourceVault.MoveFile(SourceFileName, FileName, false);
        except
          on E: Exception do
            Application.ShowException(E);
        end;
      end;
    end
    else
    begin
      Result := InternalMoveFolder(SourceFileName, NewPath);
    end;
  end;

  function InternalMoveFolder(const SourceFolderName, NewPath: String): Boolean;
  var
    FolderName: String;
    Search: Int64;
    SearchResult: Boolean;
  begin
    Result := true;
    FolderName := NewPath + chr(DestVault.PathSeparator) + ExtractFileNameEx(SourceFolderName, chr(SourceVault.PathSeparator));
    if (SourceVault = DestVault) and
       not SourceVault.FileExists(FolderName) then
    begin
      MoveFile(SourceVault, SourceFolderName, FolderName);
      Exit;
    end;

    DestVault.CreateDirectory(FolderName, true);

    Search := SourceVault.FindFirst(SourceFolderName + chr(SourceVault.PathSeparator) + '*', VAULT_FATTR_ANY_FILE, 0);
    if Search <> -1 then
      try
        SearchResult := true;
        while SearchResult do
        begin
          Result := InternalMoveFile(SourceVault.GetSearchResultFullName(Search), FolderName);
          if not Result then
            Exit;

          SearchResult := SourceVault.FindNext(Search)
        end;
      finally
        SourceVault.FindClose(Search);
      end;

    try
      SourceVault.DeleteFile(SourceFolderName);
    except
      on E: Exception do
      begin
        if (E is EcbvCBVault) and
           (EcbvCBVault(E).Code = VAULT_ERR_DIRECTORY_NOT_EMPTY) then
          // do nothing, means some files wasn't copied
        else
          Application.ShowException(E);
      end;
    end;
  end;

begin
  InternalMoveFile(SourceFileName, NewPath);
end;

procedure TFormVaultExplorer.lvFilesDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  Item: TListItem;
  i: Integer;
begin
  Accept := false;
  if (Source <> lvFiles) and
     (Source <> tvFolders) and
     (State <> dsDragLeave) then
    Exit;

  if Source = lvFiles then
  begin
    Item := lvFiles.GetItemAt(X, Y);
    if (Item = nil) or
       ((PListFileInfo(Item.Data).Attributes and VAULT_FATTR_DIRECTORY) = 0) then
      Exit;

    for i := 0 to lvFiles.Items.Count - 1 do
      if (Item = lvFiles.Items[i]) and
         lvFiles.Items[i].Selected then
        Exit;
  end;

  if Source = tvFolders then
  begin
    if not Assigned(tvFolders.Selected.Parent) then
      Exit;
  end;

  Accept := true;
end;

procedure TFormVaultExplorer.tvFoldersDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  Node: TTreeNode;
begin
  Accept := false;
  if (Source <> lvFiles) and
     (Source <> tvFolders) and
     (State <> dsDragLeave) then
    Exit;

  if Source = lvFiles then
  begin
    Node := tvFolders.GetNodeAt(X, Y);
    if (Node = nil) or (Node = FNode) then
      Exit;
  end;

  if Source = tvFolders then
  begin
    Node := tvFolders.GetNodeAt(X, Y);
    if not Assigned(tvFolders.Selected.Parent) and
       (Node.Parent <> nil) then
      Exit;
  end;

  Accept := true;
end;

procedure TFormVaultExplorer.lvFilesDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  Item: TListItem;
  i: Integer;
  FileName, OldPath, NewPath: String;
  Node: TTreeNode;
  Vault: TcbvCBVault;
begin
  ClearBuffer;

  if Source = lvFiles then
  begin
    Item := lvFiles.GetItemAt(X, Y);
    if (Item = nil) or
       ((PListFileInfo(Item.Data).Attributes and VAULT_FATTR_DIRECTORY) = 0) then
      Exit;

    OldPath := FPath + chr(FVault.PathSeparator);
    NewPath := OldPath + Item.Caption + chr(FVault.PathSeparator);
    for i := 0 to lvFiles.Items.Count - 1 do
      if lvFiles.Items[i].Selected then
      begin
        FileName := lvFiles.Items[i].Caption;
        // no special check needed
        try
          FVault.MoveFile(OldPath + FileName, NewPath + FileName, false);
        except
          on E: Exception do
            Application.ShowException(E);
        end;
      end;

    OldPath := FPath;
    UpdateNodes(FRoot);
    SelectNode(OldPath);
    Exit;
  end;

  if Source = tvFolders then
  begin
    Item := lvFiles.GetItemAt(X, Y);
    if (Item <> nil) and
       ((PListFileInfo(Item.Data).Attributes and VAULT_FATTR_DIRECTORY) = 0) then
      Item := nil;

    Node := tvFolders.Selected;
    if Node.Parent = nil then
      Exit;

    Vault := GetNodeVault(Node);
    if not Assigned(Vault) or
       not Vault.Active then
      Exit;

    OldPath := GetNodePath(Node);
    if Vault = FVault then
    begin
      NewPath := FPath;
      if Item <> nil then
        NewPath := NewPath + chr(FVault.PathSeparator) + Item.Caption;

      MoveFile(FVault, OldPath,
        NewPath + chr(FVault.PathSeparator) + ExtractFileNameEx(OldPath, chr(FVault.PathSeparator)));

      OldPath := FPath;
      UpdateNodes(FRoot);
      SelectNode(OldPath);
    end
    else
    begin
      CopyFileOptionsSeted := false;
      CopyFiles(Vault, FVault, OldPath, FPath);
      ClearFileOptions(FVault);
      UpdateNodes(Node);
    end;
  end;
end;

procedure TFormVaultExplorer.tvFoldersDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  i: Integer;
  Node, SourceNode: TTreeNode;
  Vault, SourceVault: TcbvCBVault;
  FileName, OldPath, NewPath: String;
  Res: Boolean;
begin
  ClearBuffer;

  if Source = lvFiles then
  begin
    Node := tvFolders.GetNodeAt(X, Y);
    if (Node = nil) or (Node = FNode) then
      Exit;

    Vault := GetNodeVault(Node);
    OldPath := FPath + chr(FVault.PathSeparator);
    NewPath := GetNodePath(Node);
    if Vault = FVault then
    begin
      for i := 0 to lvFiles.Items.Count - 1 do
        if lvFiles.Items[i].Selected then
        begin
          FileName := lvFiles.Items[i].Caption;
          try
            MoveFile(FVault, OldPath + FileName, NewPath + chr(FVault.PathSeparator) + FileName);
          except
            on E: Exception do
              Application.ShowException(E);
          end;
        end;

      OldPath := FPath;
      UpdateNodes(FRoot);
      SelectNode(OldPath);
    end
    else
    begin
      CopyFileOptionsSeted := false;
      for i := 0 to lvFiles.Items.Count - 1 do
        if lvFiles.Items[i].Selected then
        begin
          FileName := lvFiles.Items[i].Caption;
          CopyFiles(FVault, Vault, OldPath + FileName, NewPath);
        end;

      ClearFileOptions(Vault);
      UpdateNodes(Node);
    end;

    Exit;
  end;

  if Source = tvFolders then
  begin
    Node := tvFolders.GetNodeAt(X, Y);
    SourceNode := tvFolders.Selected;
    if (Node = nil) or (Node = SourceNode) then
      Exit;

    if not Assigned(SourceNode.Parent) and not Assigned(Node.Parent) then
    begin
      tvFolders.Items.BeginUpdate;
      try
        if SourceNode.AbsoluteIndex > Node.AbsoluteIndex then
          SourceNode.MoveTo(Node, naInsert)
        else
          Node.MoveTo(SourceNode, naInsert);

        Node.Expand(False);
        SourceNode.Expand(False);
      finally
        tvFolders.Items.EndUpdate;
      end;

      Exit;
    end;

    Vault := GetNodeVault(Node);
    SourceVault := GetNodeVault(SourceNode);
    OldPath := GetNodePath(SourceNode);
    NewPath := GetNodePath(Node);
    if Vault = SourceVault then
    begin
      NewPath := NewPath + chr(Vault.PathSeparator) + ExtractFileNameEx(OldPath, chr(Vault.PathSeparator));
      Res := MoveFile(Vault, OldPath, NewPath);

      if Res then
      begin
        SourceNode.Delete;

        UpdateFolder(Node);
        if tvFolders.Selected <> Node then
          tvFolders.Selected := Node;
      end;
    end
    else
    begin
      CopyFileOptionsSeted := false;
      CopyFiles(SourceVault, Vault, OldPath, NewPath);
      ClearFileOptions(Vault);
      UpdateFolder(Node);
      if tvFolders.Selected <> Node then
        tvFolders.Selected := Node;
    end;
  end;
end;

procedure TFormVaultExplorer.Refresh;
begin
  UpdateFolder(FNode);
end;

function TFormVaultExplorer.FindNode(RootNode: TTreeNode;
  const Path: String): TTreeNode;

  function InternalFindNode(Node: TTreeNode): TTreeNode;
  var
    i: Integer;
  begin
    if GetNodePath(Node) = Path then
    begin
      Result := Node;
      Exit;
    end;

    if Node.HasChildren and (Node.Count = 0) and
       (FindNode(RootNode, GetNodePath(Node)) = Node) then
      ExpandFolder(Node);

    Result := nil;
    for i := 0 to Node.Count - 1 do
    begin
      Result := InternalFindNode(Node.Item[i]);
      if Result <> nil then
        Break;
    end;
  end;

begin
  if RootNode <> nil then
    Result := InternalFindNode(RootNode)
  else
    Result := nil;
end;

function TFormVaultExplorer.GetNodePath(Node: TTreeNode): String;
begin
  if not Assigned(Node) or
     not Assigned(Node.Data) or
     not Assigned(Node.Parent) then
    Result := ''
  else
    Result := PWideChar(Node.Data);
end;

function TFormVaultExplorer.GetNodeFullPath(Node: TTreeNode): String;
var
  TempNode: TTreeNode;
  Vault: TcbvCBVault;
begin
  Result := '';
  if Assigned(Node) and
     Assigned(Node.Data) and
     Assigned(Node.Parent) then
  begin
    Vault := GetNodeVault(Node);
    TempNode := Node;
    while TempNode <> nil do
    begin
      Result := chr(Vault.PathSeparator) + TempNode.Text + Result;
      TempNode := TempNode.Parent;
    end;
  end;
end;

function TFormVaultExplorer.GetNodeRoot(Node: TTreeNode): TTreeNode;
begin
  Result := Node;
  if Result = nil then
    Exit;

  while Result.Parent <> nil do
    Result := Result.Parent;
end;

function TFormVaultExplorer.GetNodeVault(Node: TTreeNode): TcbvCBVault;
begin
  Node := GetNodeRoot(Node);
  if Node <> nil then
    Result := TcbvCBVault(Node.Data)
  else
    Result := nil;
end;

procedure TFormVaultExplorer.SelectNode(const Path: String);
var
  Node: TTreeNode;
begin
  if not Assigned(FRoot) then
    Exit;

  Node := FindNode(FRoot, Path);
  if Node <> nil then
  begin
    if Node.Selected then
      UpdateFiles(Node)
    else
      Node.Selected := true;
  end;
end;

procedure TFormVaultExplorer.SaveNodes(Node: TTreeNode; var NodeStates: TNodeStates);

  function CountNodes(Node: TTreeNode): Integer;
  var
    i: Integer;
  begin
    Result := 1;
    for i := 0 to Node.Count - 1 do
      Inc(Result, CountNodes(Node.Item[i]));
  end;

  var
    k: Integer;

  function WriteNodes(Node: TTreeNode): Integer;
  var
    i: Integer;
  begin
    Result := k;
    NodeStates[k].Path := GetNodeFullPath(Node);
    NodeStates[k].Expanded := Node.Expanded;
    Inc(k);
    for i := 0 to Node.Count - 1 do
      WriteNodes(Node.Item[i]);
  end;

begin
  SetLength(NodeStates, CountNodes(Node));
  k := 0;
  WriteNodes(Node);
end;

procedure TFormVaultExplorer.RestoreAndUpdateNodes(Node: TTreeNode;
  const NodeStates: TNodeStates);

  procedure RestoreNodes(Node: TTreeNode);
  var
    i, k: Integer;
    NodePath: String;
  begin
    for i := 0 to Node.Count - 1 do
    begin
      NodePath := GetNodeFullPath(Node.Item[i]);
      for k := 0 to Length(NodeStates) - 1 do
        if NodeStates[k].Path = NodePath then
        begin
          if NodeStates[k].Expanded then
          begin
            ExpandFolder(Node.Item[i]);
            Node.Item[i].Expand(False);
            RestoreNodes(Node.Item[i]);
          end;

          Break;
        end;
    end;
  end;

begin
  tvFolders.Items.BeginUpdate;
  try
    Node.DeleteChildren;
    ExpandFolder(Node);

    if not Assigned(FVault) or
       (Length(NodeStates) = 0) or
       (GetNodePath(Node) <> NodeStates[0].Path) then
      Exit;

    if NodeStates[0].Expanded or (Node.Parent = nil) then
    begin
      Node.Expand(False);
      RestoreNodes(Node);
    end;
  finally
    tvFolders.Items.EndUpdate;
  end;
end;

procedure TFormVaultExplorer.UpdateNodes(Node: TTreeNode);
var
  NodeStates: TNodeStates;
begin
  SaveNodes(Node, NodeStates);
  RestoreAndUpdateNodes(Node, NodeStates);
end;

procedure TFormVaultExplorer.SaveOpenVaults;
var
  Reg: TRegIniFile;
  i: Integer;
  RootNode: TTreeNode;
  Vault: TcbvCBVault;
begin
  Reg := TRegIniFile.Create;
  Reg.RootKey := REG_ROOTKEY;
  if Reg.OpenKey(REG_PATH, true) then
  begin
    Reg.WriteInteger('', 'ViewStyle', Integer(lvFiles.ViewStyle));
    Reg.CloseKey;
  end;

  i := 0;
  RootNode := tvFolders.Items.GetFirstNode;
  while RootNode <> nil do
  begin
    Vault := GetNodeVault(RootNode);
    if Reg.OpenKey(REG_PATH + REG_VAULTS + '\' + IntToStr(i), true) then
    begin
      Reg.WriteString('', 'FileName', Vault.VaultFile);
      Reg.WriteInteger('', 'ReadOnly', Ord(Vault.ReadOnly));
      Reg.WriteBool('', 'UseAccessTime', Vault.UseAccessTime);
      Reg.WriteInteger('', 'AutoCompact', Vault.AutoCompactAt);
      Reg.CloseKey;
    end;

    Inc(i);
    RootNode := RootNode.getNextSibling;
  end;

  while Reg.KeyExists(REG_PATH + REG_VAULTS + '\' + IntToStr(i)) do
  begin
    Reg.EraseSection(REG_PATH + REG_VAULTS + '\' + IntToStr(i));
    Inc(i);
  end;

  Reg.Free;
end;

procedure TFormVaultExplorer.lvFilesColumnClick(Sender: TObject;
  Column: TListColumn);
var
  i: Integer;
begin
  if SortSectionToCompare = Column.Tag then
    SortInverted := not SortInverted
  else
    SortInverted := false;

  SortSectionToCompare := Column.Tag;
  lvFiles.AlphaSort;

  lvFiles.Columns.BeginUpdate;
  try
    for i := 0 to lvFiles.Columns.Count - 1 do
      lvFiles.Columns[i].ImageIndex := -1;

    if SortInverted then
      Column.ImageIndex := icoSortDesc
    else
      Column.ImageIndex := icoSortAsc;

  finally
    lvFiles.Columns.EndUpdate;
  end;
end;

procedure TFormVaultExplorer.lvFilesCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var
  Sign: Integer;
begin
  if not Assigned(Item1.Data) or not Assigned(Item2.Data) then
  begin
    Compare := 0;
    Exit;
  end;

  if SortInverted then
    Sign := -1
  else
    Sign := 1;

  case SortSectionToCompare of
    scFileSize:
      if PListFileInfo(Item1.Data).FileSize < PListFileInfo(Item2.Data).FileSize then
        Compare := Sign
      else
        if PListFileInfo(Item1.Data).FileSize > PListFileInfo(Item2.Data).FileSize then
          Compare := -Sign
        else
          Compare := 0;

    scModificationTime:
      if PListFileInfo(Item1.Data).ModificationTime < PListFileInfo(Item2.Data).ModificationTime then
        Compare := Sign
      else
        if PListFileInfo(Item1.Data).ModificationTime > PListFileInfo(Item2.Data).ModificationTime then
          Compare := -Sign
        else
          Compare := 0;

    scAttributes:
      if PListFileInfo(Item1.Data).Attributes < PListFileInfo(Item2.Data).Attributes then
        Compare := Sign
      else
        if PListFileInfo(Item1.Data).Attributes > PListFileInfo(Item2.Data).Attributes then
          Compare := -Sign
        else
          Compare := 0;

  else
    if (PListFileInfo(Item1.Data).Attributes and VAULT_FATTR_DIRECTORY) <
       (PListFileInfo(Item2.Data).Attributes and VAULT_FATTR_DIRECTORY) then
      Compare := 1
    else
      if (PListFileInfo(Item1.Data).Attributes and VAULT_FATTR_DIRECTORY) >
         (PListFileInfo(Item2.Data).Attributes and VAULT_FATTR_DIRECTORY) then
        Compare := -1
      else
        Compare := Sign * WideCompareText(PListFileInfo(Item1.Data).FileName, PListFileInfo(Item2.Data).FileName);
  end;
end;

procedure TFormVaultExplorer.lvFilesDeletion(Sender: TObject;
  Item: TListItem);
begin
  FreeFileInfoData(Item.Data);
  Item.Data := nil;

end;

procedure TFormVaultExplorer.actFilterExecute(Sender: TObject);
begin
  FilterActive := not FilterActive;
  Refresh;
end;

procedure TFormVaultExplorer.actFilterUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FVault) and (cmbQuery.Text <> '');
  (Sender as TAction).Checked := FilterActive;
end;

initialization
  RegistrationKeyOK := false;

end.






