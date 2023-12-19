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
unit callbackvaultc;

interface

uses SysUtils, Classes, CBVCore, CBVCBVault, CBVConstants, Windows, IOUtils;

function Main: Integer;

implementation

type
  TIterateFilesFunction = function(const RootDir, Path: string;
    IsDirectory: Boolean; Vault: TcbvCBVault): Integer;
  TIterateVaultFunction = function(SearchData: Int64;
    Vault: TcbvCBVault): Integer;
  TCommandFunction = function(const VaultName: string;
    Paths: TStrings): Integer;

  TCommand = (cmdAdd, cmdDelete, cmdExtract_E, cmdList, cmdTest, cmdExtract_X);

  AnsiChars = set of AnsiChar;

  CBVaultEventHandler = Class
    public
      procedure OnVaultDelete(Sender: TObject; const FileName: String; var ResultCode: Integer);
      procedure OnVaultFlush(Sender: TObject; FileHandle: Int64; var ResultCode: Integer);
      procedure OnVaultRead(Sender: TObject;FileHandle: Int64;Offset: Int64;Buffer: Pointer;Count: Integer;var ResultCode: Integer);
      procedure OnVaultGetSize(Sender: TObject;FileHandle: Int64;var Size: Int64;var ResultCode: Integer);
      procedure OnVaultOpen(Sender: TObject;const FileName: String;var FileHandle: Int64; OpenMode: Integer; var ReadOnly: Boolean;var ResultCode: Integer);
      procedure OnVaultClose(Sender: TObject;FileHandle: Int64;var ResultCode: Integer);
      procedure OnVaultSetSize(Sender: TObject;FileHandle: Int64;NewSize: Int64;var ResultCode: Integer);
      procedure OnVaultWrite(Sender: TObject;FileHandle: Int64;Offset: Int64;Buffer: Pointer;Count: Integer;var ResultCode: Integer);
  End;

const
  ALL_COMMANDS: AnsiChars = ['a', 'd', 'e', 'l', 't', 'x'];
  ALL_OPTIONS: AnsiChars  = ['c', 'd', 'f', 'g', 'l', 'o', 'p', 'r', 's',
    'u', 'w', 'y', '-'];
  OPTIONS_WITH_PARAMETERS: AnsiChars = ['c', 'f', 'g', 'o', 'p', 's', 'w'];

  BUFFER_SIZE = 1024 * 1024;

  ANSWER_UNKNOWN = 0;
  ANSWER_YES = 1;
  ANSWER_NO = 2;
  ANSWER_ALL = 3;

  strInternalError = 'Internal error';

{$IFDEF MSWINDOWS}
  PathDelimiter = '\';
{$ELSE}
  PathDelimiter = '/';
{$ENDIF}
  PathDelimiters: AnsiChars = ['\', '/'];

var
  Command: TCommand;

  Handler: CBVaultEventHandler = nil;

  OptionAlwaysYes: Boolean = false;
  OptionCompressionLevel: Integer = -1;
  OptionDeleteAfterArchiving: Boolean = false;
  OptionFixedVaultSize: Int64 = 0;
  OptionLowerCase: Boolean = false;
  OptionOutputPath: string = '.';
  OptionOverwriteFiles: Boolean = false;
  OptionOverwriteFilesSet: Boolean = false;
  OptionOverwriteVault: Boolean = false;
  OptionPageSize: Integer = 0;
  OptionPassword: string = '';
  OptionRecursive: Boolean = false;
  OptionUpperCase: Boolean = false;

procedure Usage;
begin
  WriteLn('Usage: vaultmgr <command> [-<switch 1> ... -<switch N>] <vault> [<files>...]');
  WriteLn;
  WriteLn('<Commands>');
  WriteLn('  a             Add files to vault');
  WriteLn('  d             Delete files from vault');
  WriteLn('  e             Extract files from vault');
  WriteLn('  l             List contents of vault');
  WriteLn('  t             Test integrity of vault');
  WriteLn('  x             eXtract files with full pathname');

  WriteLn('<Switches>');
  WriteLn('  -             Stop switches scanning');

  WriteLn('  l             Convert names to lower case');
  WriteLn('  u             Convert names to upper case');
  WriteLn('  d             Delete files after archiving');
  WriteLn('  c<0,1,3,4>    set Compression level (0 - default)');
  WriteLn('  e             Exclude paths from names');
  // when adding files to vault
  WriteLn('  f<size>       set the Fixed size of the vault in MB');
  WriteLn('  g<page size>  set size of the paGe for new vault (4096 - default)');
  WriteLn('  o<path>       set Output directory');
  WriteLn('  p<password>   set Password');
  WriteLn('  r             Recurse subdirectories');
  WriteLn('  s+            overwrite existing Vault');
  WriteLn('  s-            do not overwrite Vault (default)');
  WriteLn('  y             Assume Yes on all queries');
  WriteLn('  w+            overWrite existing files');
  WriteLn('  w-            do not overWrite existing files (default)');
end;

function FlagOn(Flag, Flags: LongWord): Boolean;
begin
  Result := ((Flag and Flags) = Flag);
end;

function IfElse(Condition: Boolean; IfTrue, IfFalse: Variant): Variant;
begin
  if Condition then
    Result := IfTrue
  else
    Result := IfFalse;
end;

function AskYNA(const Question: string): Integer;
var
  s: string;
  Answer: Char;
begin
  Result := ANSWER_UNKNOWN;
  Write(Format('%s? (yna) ', [Question]));
  ReadLn(s);
  if (Length(s) = 1) then
  begin
    Answer := s[1];
    case Answer of
      'y', 'Y':
        Result := ANSWER_YES;
      'n', 'N':
        Result := ANSWER_NO;
      'a', 'A':
        Result := ANSWER_ALL;
    end;
  end;
end;

function Error2Str(ErrorCode: Integer; VaultMessage : string): string;
begin
  if ErrorCode < 0 then
  begin
    if ((ErrorCode < 0) and
      (ErrorCode >= CBVConstants.VAULT_ERR_EXTERNAL_ERROR)) then
      Result := VaultMessage
    else
      Result := strInternalError;
    Result := Result + Format('(Error code = %d)', [ErrorCode]);
  end
  else
  if Length(VaultMessage) > 0 then
    Result := Format('%s (Error code = %d)', [VaultMessage, ErrorCode])
  else
    Result := Format('%s (Error code = %d)', [SysErrorMessage(ErrorCode), ErrorCode]);
end;

function SetOption(Option: AnsiChar; Parameter: string): Boolean;
begin
  Result := true;
  case Option of
    'c':
      begin
        if not TryStrToInt(Parameter, OptionCompressionLevel) then
        begin
          Result := false;
        end;
        if not(OptionCompressionLevel in [0..9]) then
        begin
          Result := false;
        end;
      end;
    'd':
      begin
        OptionDeleteAfterArchiving := true;
      end;
    'f':
      begin
        if not TryStrToInt64(Parameter, OptionFixedVaultSize) then
          Result := false;
        if OptionFixedVaultSize <= 0 then
          Result := false;
        OptionFixedVaultSize := OptionFixedVaultSize * 1024 * 1024;
      end;
    'g':
      begin
        if not TryStrToInt(Parameter, OptionPageSize) then
          Result := false;
        if OptionPageSize < 512 then
          Result := false;
      end;
    'l':
      begin
        OptionLowerCase := true;
      end;
    'o':
      begin
        OptionOutputPath := ConvertRelativePathToAbsolute(Parameter);
      end;
    'p':
      begin
        OptionPassword := Parameter;
      end;
    'r':
      begin
        OptionRecursive := true;
      end;
    's':
      begin
        if Length(Parameter) <> 1 then
        begin
          Result := false;
        end;
        case Parameter[1] of
          '+':
            OptionOverwriteVault := true;
          '-':
            OptionOverwriteVault := false;
        else
          Result := false;
        end;
      end;
    'u':
      begin
        OptionUpperCase := true;
      end;
    'w':
      begin
        if Length(Parameter) <> 1 then
        begin
          Result := false;
        end;
        case Parameter[1] of
          '+':
            begin
              OptionOverwriteFiles := true;
              OptionOverwriteFilesSet := true;
            end;
          '-':
            begin
              OptionOverwriteFiles := false;
              OptionOverwriteFilesSet := true;
            end;
        else
          Result := false;
        end;
      end;
    'y':
      begin
        OptionAlwaysYes := true;
      end;
  else
    begin
      Result := false;
    end;
  end;
end;

function ConvertRelativePathToAbsolute(const path: string): string;
var
  res: string;
  homeDir: string;
begin
  res := path;
  if not path.IsEmpty then
  begin
    if (path = '~') or StartsText('~/', path) then
    begin
      homeDir := GetEnvironmentVariable('HOME');
      if path = '~' then
        Exit(homeDir)
      else
        Exit(homeDir + Copy(path, 2, MaxInt));
    end
    else if not IsDriveLetter(path) then
    begin
      try
        res := TPath.GetFullPath(path);

        if StartsText('\\', res) and not DirectoryExists(res) then
        begin
          WriteLn('The network folder "', res, '" does not exist.');
        end
        else if not FileExists(res) and not DirectoryExists(res) then
        begin
          WriteLn('The path "', res, '" does not exist.');
        end;
      except
        on E: Exception do
          WriteLn('Error while converting to absolute path: ', E.Message);
      end;
    end;
  end;
  Result := res;
end;

function IsDriveLetter(const path: string): Boolean;
begin
  Result := False;

  if (path <> '') and (not path.IsEmpty) then
  begin
    if (path[1] in ['A'..'Z', 'a'..'z']) and (Length(path) = 2) and (path[2] = ':') then
      Result := True;
  end;
end;

//------------------------------------------------------------------------------
procedure CBVaultEventHandler.OnVaultDelete(Sender: TObject; const FileName: String; var ResultCode: Integer);
begin
  DeleteFile(PChar(FileName));
  ResultCode := 0;
end;

procedure CBVaultEventHandler.OnVaultFlush(Sender: TObject; FileHandle: Int64; var ResultCode: Integer);
begin
  ResultCode := 0;
end;

procedure CBVaultEventHandler.OnVaultRead(Sender: TObject;FileHandle: Int64;Offset: Int64;Buffer: Pointer;Count: Integer;var ResultCode: Integer);
var
FileStream: TFileStream;
begin
  try
    FileStream := TFileStream(FileHandle);
    FileStream.Seek(Offset, soBeginning);
    FileStream.ReadBuffer(PChar(Buffer)^, Count);
    ResultCode := 0;
  except
  on E : Exception do
    begin
    ResultCode := GetLastError();
    end; 
  end;
end;

procedure CBVaultEventHandler.OnVaultGetSize(Sender: TObject;FileHandle: Int64;var Size: Int64;var ResultCode: Integer);
var
FileStream: TFileStream;
begin
  try
    FileStream := TFileStream(FileHandle);
    Size := FileStream.Size;
    ResultCode := 0;
  except
  on E : Exception do
    begin
    ResultCode := GetLastError();
    end; 
  end;
end;

procedure CBVaultEventHandler.OnVaultOpen(Sender: TObject;const FileName: String;var FileHandle: Int64; OpenMode: Integer; var ReadOnly: Boolean;var ResultCode: Integer);
var
FileStream: TFileStream;
begin
  try
    if (OpenMode = VAULT_OM_CREATE_NEW) or (OpenMode = VAULT_OM_CREATE_ALWAYS) then
      FileStream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite)
    else FileStream := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyWrite);
    FileHandle := Int64(FileStream);
    ResultCode := 0;
  except
  on E : Exception do
    begin
    ResultCode := GetLastError();
    end; 
  end; 
end;

procedure CBVaultEventHandler.OnVaultClose(Sender: TObject;FileHandle: Int64;var ResultCode: Integer);
var
FileStream: TFileStream;
begin
  try
    FileStream := TFileStream(FileHandle);
    FileStream.Free();
    ResultCode := 0;
  except
  on E : Exception do
    begin
    ResultCode := GetLastError();
    end; 
  end;
end;

procedure CBVaultEventHandler.OnVaultSetSize(Sender: TObject;FileHandle: Int64;NewSize: Int64;var ResultCode: Integer);
var
FileStream: TFileStream;
begin
  try
    FileStream := TFileStream(FileHandle);
    FileStream.Size := NewSize;
    ResultCode := 0;    
  except
  on E : Exception do
    begin
    ResultCode := GetLastError();
    end; 
  end;
end;

procedure CBVaultEventHandler.OnVaultWrite(Sender: TObject;FileHandle: Int64;Offset: Int64;Buffer: Pointer;Count: Integer;var ResultCode: Integer);
var
FileStream: TFileStream;
begin
  try
    FileStream := TFileStream(FileHandle);
    FileStream.Seek(Offset, soBeginning);
    FileStream.WriteBuffer(PChar(Buffer)^, Count);
    ResultCode := 0;
  except
  on E : Exception do
    begin
    ResultCode := GetLastError();
    end; 
  end;
end;
//-----------------------------------------------------------------------------

function OpenVault(const VaultName: string; var Vault: TcbvCBVault;
  ExistingVaultOnly: Boolean): Integer;
var ErrMessage : string;
begin
  Result := 0;
  try
    Vault := TcbvCBVault.Create(nil);
    Vault.CallbackMode := true;
    begin
       Handler := CBVaultEventHandler.Create();
       Vault.OnVaultOpen := Handler.OnVaultOpen;
       Vault.OnVaultClose := Handler.OnVaultClose;
       Vault.OnVaultDelete := Handler.OnVaultDelete;
       Vault.OnVaultFlush := Handler.OnVaultFlush;
       Vault.OnVaultGetSize := Handler.OnVaultGetSize;
       Vault.OnVaultSetSize := Handler.OnVaultSetSize;
       Vault.OnVaultRead := Handler.OnVaultRead;
       Vault.OnVaultWrite := Handler.OnVaultWrite;
    end;
    Vault.VaultFile := VaultName;
    Vault.PathSeparator := Ord(PathDelimiter);

    if ExistingVaultOnly then
    begin
      Vault.OpenVault(CBVConstants.VAULT_OM_OPEN_EXISTING, CBVConstants.VAULT_JM_NONE);
    end
    else if OptionOverwriteVault or not FileExists(VaultName) then
    begin
      if OptionPageSize <> 0 then
      begin
        Vault.PageSize := OptionPageSize;
      end;
      if OptionFixedVaultSize <> 0 then
      begin
        Vault.VaultSizeMax := OptionFixedVaultSize;
        Vault.VaultSizeMin := OptionFixedVaultSize;
      end;
      Vault.OpenVault(CBVConstants.VAULT_OM_CREATE_ALWAYS, CBVConstants.VAULT_JM_NONE);
    end
    else
    begin
      Vault.OpenVault(CBVConstants.VAULT_OM_OPEN_ALWAYS, CBVConstants.VAULT_JM_NONE);
    end;
  except
    on E: ECBFSVault do
      begin
        Result := E.Code;
        ErrMessage := E.Message;
      end;
    on E : Exception do
    begin
      Result := GetLastError();
      ErrMessage := E.Message;
    end;
  end;
  if (Result <> 0) then
  begin
    WriteLn(Format('Error opening vault: %s', [Error2Str(Result, ErrMessage)]));
    Exit;
  end;
end;

// C:\Test1\Test2\test.txt -> Test1\Test2
function ExtractFileDirectory(const FileName: string): string;
var
  FileDrive, FilePath: string;
begin
  FileDrive := ExtractFileDrive(FileName);
  if Length(FileDrive) > 0 then
    FileDrive := IncludeTrailingPathDelimiter(FileDrive);
  FilePath := ExtractFileDir(FileName);
  Result := Copy(FilePath, Length(FileDrive) + 1, MaxInt);
end;

// C:\Test1\Test2\test.txt -> \Test1\Test2\test.txt
function RemoveFileDrive(const FileName: string): string;
var
  FileDrive: string;
begin
  FileDrive := ExtractFileDrive(FileName);
  Result := Copy(FileName, Length(FileDrive) + 1, MaxInt);
end;

function IncludeLeadingPathDelimiterEx(const Path: String): String;
begin
  if Length(Path) = 0 then
    Result := PathDelimiter
  else
  begin
    if AnsiChar(Path[1]) in PathDelimiters then
      Result := Copy(Path, 2, MaxInt)
    else
      Result := Path;
    Result := PathDelimiter + Result
  end;
end;

function LastChar(Chars: AnsiChars; const S: String): Integer;
begin
  Result := Length(S);
  while (Result > 0) and not (AnsiChar(S[Result]) in Chars) do
    Dec(Result);
end;

function ExcludeTrailingPathDelimiterEx(const Path: String): String;
begin
  Result := Path;
  if AnsiChar(Result[Length(Result)]) in PathDelimiters then
    SetLength(Result, Length(Result) - 1);
end;

function IncludeTrailingPathDelimiterEx(const Path: String): String;
begin
  Result := Path;
  if not (AnsiChar(Result[Length(Result)]) in PathDelimiters) then
    Result := Result + PathDelimiter;
end;

function ExtractFileNameEx(const FileName: String): String;
var
  I: Integer;
begin
  I := LastChar(PathDelimiters, FileName);
  Result := Copy(FileName, I, MaxInt);
end;

function ExtractFilePathEx(const FileName: String): String;
var
  I: Integer;
begin
  I := LastChar(PathDelimiters, FileName);
  Result := Copy(FileName, 1, I - 1);
end;

procedure SplitPathWithMaskEx(const PathWithMask: string;
  var Dir: string; var Mask: string);
begin
  if (Pos('*', PathWithMask) > 0) or (Pos('?', PathWithMask) > 0) then
  begin
    Dir := ExtractFilePathEx(PathWithMask);
    Mask := ExtractFileNameEx(PathWithMask);
  end
  else
  begin
    Dir := PathWithMask;
    Mask := '';
  end;
end;

function OSPathToVaultPath(const Path: string): string;
begin
  Result := RemoveFileDrive(Path);
  while true do
  begin
    if (Pos('./', Result) = 1) or (Pos('.\', Result) = 1) then
      Result := Copy(Result, 3)
    else if (Pos('../', Result) = 1) or (Pos('..\', Result) = 1) then
      Result := Copy(Result, 4)
    else
      break;
  end;
  if (Result = '.') or (Result = '..') then
      Result := PathDelimiter;
  if (Length(Result) >= 1) and (Result[1] <> PathDelimiter) then
    Result := PathDelimiter + Result;
  if OptionLowerCase then
    Result := AnsiLowerCaseFileName(Result);
  if OptionUpperCase then
    Result := AnsiUpperCaseFileName(Result);
end;
(*
// Checks for '..\' and '.\' prefixes
// deletes them and changes current directory
function ProceedRelativePath(const Path: string): string;
  procedure MoveUp;
  var
    Path: string;
  begin
    Path := ExcludeTrailingPathDelimiter(GetCurrentDir);
    Path := ExtractFilePath(Path);
    if not SetCurrentDir(Path) then
      RaiseLastOSError;
  end;

begin
  Result := Path;
  while (Length(Result) > 1) and (Result[1] = '.') do
  begin
    if (Length(Result) >= 2) and (Result[2] = '\') then
      Result := Copy(Result, 3, MaxInt)
    else if (Length(Result) >= 3) and (Result[2] = '.') and (Result[3] = '\')
    then
    begin
      MoveUp;
      Result := Copy(Result, 4, MaxInt)
    end
    else
      Break;
  end;
end;
*)
function MakePath(const FirstPart, SecondPart: string): string;
begin
  Result := ExcludeTrailingPathDelimiterEx(FirstPart) +
    IncludeLeadingPathDelimiterEx(SecondPart);
end;
(*
function MakeAbsolutePath(const Path: string): string;
var
  CurrentDir: string;
begin
  CurrentDir := GetCurrentDir;
  try
    Result := ProceedRelativePath(Path);
    if ExtractFileDrive(Result) = '' then
    begin
      Result := MakePath(GetCurrentDir, Result, '\');
    end;
  finally
    SetCurrentDir(CurrentDir);
  end;
end;
*)
function AddDirectory(Vault: TcbvCBVault;
  const DirectoryName: string): Integer;
var
  VaultDirectoryName, ErrMessage: string;
begin
  Result := 0;
  VaultDirectoryName := OSPathToVaultPath(DirectoryName);
  if not Vault.FileExists(VaultDirectoryName) then
  begin
    Write(Format('Adding dir  %s', [VaultDirectoryName]));
    try
      Vault.CreateDirectory(VaultDirectoryName, true);
    except
      on E: ECBFSVault do
      begin
        Result := E.Code;
        ErrMessage := E.Message;
      end;
      on E: Exception do
      begin
        Result := GetLastError();
        ErrMessage := E.Message;
      end;
    end;
    if Result <> 0 then
      WriteLn(Format(' - error: %s', [Error2Str(Result, ErrMessage)]))
    else
      WriteLn(' - ok');
  end;
end;

function AddFile(Vault: TcbvCBVault; const FileName: string): Integer;
var
  FileStream: TFileStream;
  AddFile: Boolean;
  VaultFileName: string;
  VaultStream: TCBFSVaultStream;
  Buffer: PByte;
  BytesRead: Integer;
  TotalRead: Int64;
  ErrMessage : string;
begin
  Result := 0;
  AddFile := true;
  VaultFileName := OSPathToVaultPath(FileName);

  AddDirectory(Vault, ExtractFilePathEx(VaultFileName));

  if Vault.FileExists(VaultFileName) then
  begin
    if OptionAlwaysYes then
      AddFile := true
    else if OptionOverwriteFilesSet then
    begin
      if OptionOverwriteFiles then
        AddFile := true
      else
      begin
        WriteLn(Format('File %s already exists. Skipped', [FileName]));
        AddFile := false;
      end;
    end
    else
    begin
      case AskYNA(Format('File %s already exists. Overwrite',
        [FileName])) of
        ANSWER_YES:
          AddFile := true;
        ANSWER_ALL:
          begin
            AddFile := true;
            OptionOverwriteFilesSet := true;
            OptionOverwriteFiles := true;
          end;
      else // ANSWER_UNKNOWN, ANSWER_NO
        begin
          AddFile := false;
          WriteLn('Skipped');
        end;
      end;
    end;
    if AddFile then
      Vault.DeleteFile(VaultFileName);
  end;

  if AddFile then
  begin
    Write(Format('Adding file %s', [VaultFileName]));
    try
      FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
      try
        VaultStream := Vault.OpenFileEx(OSPathToVaultPath(FileName), cbvConstants.VAULT_FOM_CREATE_NEW, false, true, true, true,
          IfElse(OptionPassword = '', CBVConstants.VAULT_EM_NONE, CBVConstants.VAULT_EM_DEFAULT),
          OptionPassword,
          IfElse(OptionCompressionLevel = -1, CBVConstants.VAULT_CM_NONE, CBVConstants.VAULT_CM_ZLIB),
          OptionCompressionLevel, 1);
        try
          Buffer := AllocMem(BUFFER_SIZE);
          try
            TotalRead := 0;
            while TotalRead < FileStream.Size do
            begin
              BytesRead := FileStream.Read(Buffer^, BUFFER_SIZE);
              Inc(TotalRead, BytesRead);
              VaultStream.Write(Buffer^, BytesRead);
            end;
          finally
            FreeMem(Buffer);
          end;
        finally
          FreeAndNil(VaultStream);
        end;
      finally
        FreeAndNil(FileStream);
      end;
    except
      on E: ECBFSVault do
      begin
        Result := E.Code;
        ErrMessage := E.Message;
      end;
      on E: Exception do
      begin
        Result := GetLastError();
        ErrMessage := E.Message;
      end;
    end;
    if Result <> 0 then
      WriteLn(Format(' - error: %s', [Error2Str(Result, ErrMessage)]))
    else
      WriteLn(' - ok');
    end;
end;

function AddFileObject(const RootDir, Path: string;
    IsDirectory: Boolean; Vault: TcbvCBVault): Integer;
var
  OSFileName: string;
begin
  OSFileName := MakePath(RootDir, Path);
  if IsDirectory then
  begin
    Result := AddDirectory(Vault, OSFileName);
    if (Result = 0) and OptionDeleteAfterArchiving then
    begin
      if not RemoveDir(OSFileName) then
        Result := GetLastError;
    end;
  end
  else
  begin
    Result := AddFile(Vault, OSFileName);
    if (Result = 0) and OptionDeleteAfterArchiving then
    begin
      if not DeleteFile(OSFileName) then
        Result := GetLastError;
    end;
  end;
end;

function IterateFilesWithRoot(const RootDir, Path, Mask: string;
  IterateFunc: TIterateFilesFunction; Vault: TcbvCBVault): Integer;
var
  FilePath: string;
  Search: TSearchRec;
  IsDirectory: boolean;
begin
  Result := FindFirst(MakePath(MakePath(RootDir, Path), Mask), faAnyFile, Search);
  if Result = 0 then
  begin
    repeat
      if (Search.Name <> '.') and (Search.Name <> '..') then
      begin
        FilePath := MakePath(Path, Search.Name);
        IsDirectory := faDirectory and Search.Attr <> 0;
        if IsDirectory then
        begin
          Result := AddDirectory(Vault, FilePath);
          if (Result = 0) and OptionRecursive then
            Result := IterateFilesWithRoot(RootDir, FilePath, Mask, IterateFunc, Vault);
        end;
        if Result = 0 then
          Result := IterateFunc(RootDir, FilePath, IsDirectory, Vault);
      end;
      if Result = 0 then
        Result := FindNext(Search);
    until Result <> 0;
    FindClose(Search);
  end;
  if (Result = 18(*No more files*)) or (Result = 2(*File not found*)) then
    Result := 0;
end;

function IterateFiles(const Path: string; IterateFunc: TIterateFilesFunction;
  Vault: TcbvCBVault): Integer;
var
  RootDir, Mask: string;
  MaskSpecified, IsDirectory, ProceedFilePath: Boolean;
begin
  Result := 0;
  ProceedFilePath := false;
  SplitPathWithMaskEx(Path, RootDir, Mask);
  if RootDir.IsEmpty then
    RootDir := '.';
  MaskSpecified := not Mask.IsEmpty;

  if not MaskSpecified then
  begin
    IsDirectory := DirectoryExists(Path);
    if IsDirectory then
    begin
      Mask := '*';
      MaskSpecified := true;
    end;
    ProceedFilePath := not IsDirectory;
  end;

  if MaskSpecified then
  begin
    Result := IterateFilesWithRoot(RootDir, '.', Mask, IterateFunc, Vault);
  end;

  if (Result = 0) and ProceedFilePath then
  begin
    Result := IterateFunc('.', Path, false, Vault);
  end;
end;

function AddFiles(const VaultName: string; Paths: TStrings): Integer;
var
  Vault: TcbvCBVault;
  Path: string;
  I: Integer;
begin
  Result := OpenVault(VaultName, Vault, false);
  if Result = 0 then
  try
    for I := 0 to Paths.Count - 1 do
    begin
      Path := Paths[I];
      Result := IterateFiles(Path, AddFileObject, Vault);
      if (Result <> 0) then
        Break;
    end;
  finally
    FreeAndNil(Vault);
  end;
end;

function IterateVault(Vault: TcbvCBVault; const Path: String;
  IterateFunc: TIterateVaultFunction; ProcessSubfolders: Boolean;
  Parameter: Pointer): Integer;
var
  FileAttributes: LongWord;
  IsDirectory: Boolean;
  SearchData: Int64;
  SearchTemplate: String;
  MaskSpecified: Boolean;
  Mask: String;
  FoundName: String;
begin
  try
    IsDirectory := false;
    SearchTemplate := ExcludeTrailingPathDelimiterEx(Path);

    MaskSpecified := (Pos('*', SearchTemplate) > 0) or
      (Pos('?', SearchTemplate) > 0);
    if not MaskSpecified then
    begin
      FileAttributes := Vault.GetFileAttributes(SearchTemplate);
      IsDirectory := FlagOn(CBVConstants.VAULT_FATTR_DIRECTORY,
        FileAttributes);
      if IsDirectory and ProcessSubfolders then
      begin
        SearchTemplate := SearchTemplate + Char(Vault.PathSeparator) + '*';
      end;
    end;

    Mask := ExtractFileNameEx(SearchTemplate);

    SearchData := Vault.FindFirst(SearchTemplate,
      CBVConstants.VAULT_FATTR_ANY_FILE, 0);
    if SearchData <> -1 then
    begin
      try
        repeat
          FoundName := Vault.GetSearchResultFullName(SearchData);
          if FlagOn(CBVConstants.VAULT_FATTR_DIRECTORY,
            Vault.GetSearchResultAttributes(SearchData))
            and ProcessSubfolders then
          begin
            Result := IterateVault(Vault,
              IncludeTrailingPathDelimiterEx(FoundName) + Mask, IterateFunc,
              ProcessSubfolders, Parameter);
            if (Result <> 0) or (Result = CBVConstants.VAULT_ERR_FILE_NOT_FOUND) then
              Break;
          end;

          Result := IterateFunc(SearchData, Vault);
          if (Result <> 0) then
            Break;
        until not Vault.FindNext(SearchData);
      finally
        Vault.FindClose(SearchData);
      end;
    end
    else
    begin
      Result := CBVConstants.VAULT_ERR_FILE_NOT_FOUND;
    end;

    if IsDirectory then
    begin
      SearchData := Vault.FindFirst(Path,
        CBVConstants.VAULT_FATTR_ANY_FILE, 0);
      if SearchData <> -1 then
      begin
        try
          Result := IterateFunc(SearchData, Vault);
        finally
          Vault.FindClose(SearchData);
        end;
      end;
    end;
  except
    on E: ECBFSVault do
    begin
      Result := E.Code;
    end;
    on E: Exception do
    begin
      Result := GetLastError();
    end;
  end;
end;

function DeleteFileObject(SearchData: Int64; Vault: TcbvCBVault): Integer;
var
  FullName: string;
  ErrMessage : string;

begin
  Result := 0;
  try
    FullName := Vault.GetSearchResultFullName(SearchData);
    if FlagOn(CBVConstants.VAULT_FATTR_DIRECTORY, Vault.GetSearchResultAttributes(SearchData)) then
      Write(Format('Deleting dir  %s', [FullName]))
    else
      Write(Format('Deleting file %s', [FullName]));
    Vault.DeleteFile(FullName);
  except
    on E: ECBFSVault do
    begin
      Result := E.Code;
      ErrMessage := E.Message;
    end;
    on E: Exception do
    begin
      Result := GetLastError();
      ErrMessage := E.Message;
    end;
  end;
  if Result <> 0 then
    WriteLn(Format(' - error: %s', [Error2Str(Result, ErrMessage)]))
  else
    WriteLn(' - ok');
  Result := 0;
end;

function DeleteFiles(const VaultName: string; Paths: TStrings): Integer;
var
  Vault: TcbvCBVault;
  I: Integer;
begin
  Result := OpenVault(VaultName, Vault, true);
  if (Result <> 0) then
    Exit;
  try
    for I := 0 to Paths.Count - 1 do
    begin
      Result := IterateVault(Vault, Paths[I], DeleteFileObject,
        true, Vault);
      if (Result <> 0) then
        Break;
    end;
  finally
    FreeAndNil(Vault);
  end;
end;

function ListFile(SearchData: Int64; Vault: TcbvCBVault): Integer;
begin
  if FlagOn(CBVConstants.VAULT_FATTR_DIRECTORY,
    Vault.GetSearchResultAttributes(SearchData)) then
    WriteLn(Format('%s', [Vault.GetSearchResultFullName(SearchData)]))
  else
    WriteLn(Format('%-68s%12u', [Vault.GetSearchResultFullName(SearchData),
      Vault.GetSearchResultSize(SearchData)]));
  Result := 0;
end;

function ExtractFile(Vault: TcbvCBVault; SearchData: Int64): Integer;
var
  FileStream: TFileStream;
  VaultStream: TCBFSVaultStream;
  OSPath: string;
  OSFileName: string;
  ExtractThisFile: Boolean;
  Buffer: PByte;
  BytesRead: Integer;
  TotalRead: Int64;
  ErrMessage : string;
begin
  Result := 0;
  try
    if (Command = cmdExtract_X) then
    begin
      OSFileName := Vault.GetSearchResultFullName(SearchData);
    end
    else if (Command = cmdExtract_E) then
    begin
      OSFileName := Vault.GetSearchResultName(SearchData);
    end
    else
      raise Exception.Create
        (Format('ExtractFileFromVault error. Invalid command: %u',
        [Ord(Command)]));

    OSFileName := MakePath(OptionOutputPath, OSFileName);
    OSPath := ExtractFilePathEx(OSFileName);

    if FileExists(OSFileName) then
    begin
      if OptionAlwaysYes then
        ExtractThisFile := true
      else if OptionOverwriteFilesSet then
      begin
        if OptionOverwriteFiles then
          ExtractThisFile := true
        else
        begin
          WriteLn(Format('File %s already exists. Skipped', [OSFileName]));
          ExtractThisFile := false;
        end;
      end
      else
      begin
        case AskYNA(Format('File %s already exists. Overwrite',
          [OSFileName])) of
          ANSWER_YES:
            ExtractThisFile := true;
          ANSWER_ALL:
            begin
              ExtractThisFile := true;
              OptionOverwriteFilesSet := true;
              OptionOverwriteFiles := true;
            end;
        else // ANSWER_UNKNOWN, ANSWER_NO
          begin
            ExtractThisFile := false;
            WriteLn('Skipped');
          end;
        end;
      end;

      if not ExtractThisFile then
        Exit;
    end;

    Write(Format('Extracting file %s',
      [Vault.GetSearchResultFullName(SearchData)]));
    if (OSPath <> '') then
      ForceDirectories(OSPath);

    VaultStream := Vault.OpenFile
      (Vault.GetSearchResultFullName(SearchData),
      CBVConstants.VAULT_OM_OPEN_EXISTING, true, false,
      OptionPassword);

    try
      FileStream := TFileStream.Create(OSFileName, fmCreate);
      try
        Buffer := AllocMem(BUFFER_SIZE);
        try
          TotalRead := 0;
          while TotalRead < VaultStream.Size do
          begin
            BytesRead := VaultStream.Read(Buffer^, BUFFER_SIZE);
            Inc(TotalRead, BytesRead);
            FileStream.Write(Buffer^, BytesRead);
          end;
        finally
          FreeMem(Buffer);
        end;
      finally
        FreeAndNil(FileStream);
      end;
    finally
      FreeAndNil(VaultStream);
    end;
  except
    on E: ECBFSVault do
    begin
      Result := E.Code;
      ErrMessage := E.Message;
    end;
    on E: Exception do
    begin
      Result := GetLastError();
      ErrMessage := E.Message;
    end;
  end;
  if (Result = 0) then
    WriteLn(' - ok')
  else
    WriteLn(Format(' - Error: %s', [Error2Str(Result, ErrMessage)]));
end;

function ExtractDirectory(Vault: TcbvCBVault; SearchData: Int64): Integer;
var
  OSDirectoryName: string;
  FullName: string;
  ErrMessage : string;
begin
  Result := 0;
  if (Command = cmdExtract_E) then
    Exit;

  FullName := Vault.GetSearchResultFullName(SearchData);
  OSDirectoryName := MakePath(OptionOutputPath, FullName);

  if not DirectoryExists(OSDirectoryName) then
  begin
    try
      Write(Format('Extracting dir  %s', [FullName]));
      ForceDirectories(OSDirectoryName);
    except
      on E: ECBFSVault do
      begin
        Result := E.Code;
        ErrMessage := E.Message;
      end;
      on E: Exception do
      begin
        Result := GetLastError();
        ErrMessage := E.Message;
      end;
    end;
    if (Result = 0) then
      WriteLn(' - ok')
    else
      WriteLn(Format(' - Error: %s', [Error2Str(Result, ErrMessage)]));
  end;
end;

function ExtractFileObject(SearchData: Int64; Vault: TcbvCBVault): Integer;
begin
  if FlagOn(CBVConstants.VAULT_FATTR_DIRECTORY,
    Vault.GetSearchResultAttributes(SearchData)) then
    Result := ExtractDirectory(Vault, SearchData)
  else
    Result := ExtractFile(Vault, SearchData);
end;

function ExtractFiles(const VaultName: string; Paths: TStrings): Integer;
var
  Vault: TcbvCBVault;
  I: Integer;
begin
  Result := OpenVault(VaultName, Vault, true);
  if (Result <> 0) then
    Exit;
  try
    if Paths.Count = 0 then
      Paths.Add('*');
    //_OutputPath := MakeAbsolutePath(OptionOutputPath);
    for I := 0 to Paths.Count - 1 do
    begin
      Result := IterateVault(Vault, Paths[I], ExtractFileObject,
        true, Vault);
      if (Result <> 0) then
        Break;
    end;
  finally
    FreeAndNil(Vault);
  end;
end;

function ListFiles(const VaultName: string; Paths: TStrings): Integer;
var
  Vault: TcbvCBVault;
  I: Integer;
begin
  Result := OpenVault(VaultName, Vault, true);
  if (Result <> 0) then
    Exit;
  try
    WriteLn(Format('%-68s%12s', ['File Name', 'Size']));
    WriteLn(StringOfChar('-', 80));
    if Paths.Count = 0 then
      Paths.Add('*');
    for I := 0 to Paths.Count - 1 do
    begin
      Result := IterateVault(Vault, Paths[I], ListFile, true, nil);
      if (Result <> 0) then
        Break;
    end;
  finally
    FreeAndNil(Vault);
  end;
end;

function TestFile(SearchData: Int64; Vault: TcbvCBVault): Integer;
var
  VaultStream: TCBFSVaultStream;
  Buffer: PByte;
  BytesRead: Integer;
  TotalRead: Int64;
  FullName: string;
  ErrMessage : string;
begin
  Result := 0;
  FullName := Vault.GetSearchResultFullName(SearchData);
  if FlagOn(CBVConstants.VAULT_FATTR_DIRECTORY,
    Vault.GetSearchResultAttributes(SearchData)) then
    Exit;
  try
    Write(Format('Tesing file %s', [FullName]));
    VaultStream := Vault.OpenFile(FullName,
      CBVConstants.VAULT_OM_OPEN_EXISTING, true, false,
      OptionPassword);
    try
      Buffer := AllocMem(BUFFER_SIZE);
      try
        TotalRead := 0;
        while TotalRead < VaultStream.Size do
        begin
          BytesRead := VaultStream.Read(Buffer^, BUFFER_SIZE);
          Inc(TotalRead, BytesRead);
        end;
      finally
        FreeMem(Buffer);
      end;
    finally
      FreeAndNil(VaultStream);
    end;
  except
    on E: ECBFSVault do
    begin
      Result := E.Code;
      ErrMessage := E.Message;
    end;
    on E: Exception do
    begin
      Result := GetLastError();
      ErrMessage := E.Message;
    end;
  end;
  if Result <> 0 then
    WriteLn(Format(' - error: %s', [Error2Str(Result, ErrMessage)]))
  else
    WriteLn(' - ok');
end;

function TestFiles(const VaultName: string; Paths: TStrings): Integer;
var
  Vault: TcbvCBVault;
  I: Integer;
begin
  Result := OpenVault(VaultName, Vault, true);
  if (Result <> 0) then
    Exit;
  try
    if Paths.Count = 0 then
      Paths.Add('*');
    for I := 0 to Paths.Count - 1 do
    begin
      Result := IterateVault(Vault, Paths[I], TestFile, true, nil);
      if (Result <> 0) then
        Break;
    end;
  finally
    FreeAndNil(Vault);
  end;
end;

function Main: Integer;
var
  InputCommand: AnsiChar;
  Option: AnsiChar;
  Parameter: string;
  ParNo: Integer;
  VaultName: string;
  Files: TStrings;
  SkipOptions: Boolean;
  CommandFunction: TCommandFunction;
begin
  Result := 0;
  if FindCmdLineSwitch('?') or (ParamCount < 2) then
  begin
    Usage;
    Exit;
  end;

  if (Length(ParamStr(1)) <> 1) then
  begin
    WriteLn(Format('Invalid command "%s"', [ParamStr(1)]));
    Exit;
  end;
  // InputCommand
  InputCommand := AnsiChar(LowerCase(ParamStr(1))[1]);
  if not (InputCommand in ALL_COMMANDS) then
  begin
    WriteLn(Format('Invalid command "%s"', [ParamStr(1)]));
    Exit;
  end;

  // options
  ParNo := 2;
  SkipOptions := false;
  while ParNo <= ParamCount do
  begin
    Parameter := ParamStr(ParNo);

    if (Length(Parameter) < 2) or (Parameter[1] <> '-') then
      Break;

    Option := AnsiChar(Parameter[2]);

    if Option = '-' then
      SkipOptions := true;

    if SkipOptions then
    begin
      Inc(ParNo);
      Continue;
    end;

    if not(Option in ALL_OPTIONS) then
    begin
      WriteLn(Format('Invalid option "%s"', [Parameter]));
      Exit;
    end;

    if (Option in OPTIONS_WITH_PARAMETERS) then
    begin
      if (Length(Parameter) < 3) or not SetOption(Option,
        Copy(Parameter, 3, Length(Parameter))) then
      begin
        WriteLn(Format('Invalid option "%s"', [Parameter]));
        Exit;
      end;
    end
    else
    begin
      if not SetOption(Option, '') then
      begin
        WriteLn(Format('Invalid option "%s"', [Parameter]));
        Exit;
      end;
    end;
    Inc(ParNo);
  end;

  if ParNo > ParamCount then
  begin
    Usage;
    Exit;
  end;

  // vault
  VaultName := ConvertRelativePathToAbsolute(ParamStr(ParNo));

  // files
  Files := TStringList.Create;
  try
    for ParNo := ParNo + 1 to ParamCount do
    begin
      Files.Add(ParamStr(ParNo));
    end;

    case InputCommand of
      'a':
        begin
          Command := cmdAdd;
          CommandFunction := AddFiles;
        end;
      'd':
        begin
          Command := cmdDelete;
          CommandFunction := DeleteFiles;
        end;
      'e':
        begin
          Command := cmdExtract_E;
          CommandFunction := ExtractFiles;
        end;
      'l':
        begin
          Command := cmdList;
          CommandFunction := ListFiles;
        end;
      't':
        begin
          Command := cmdTest;
          CommandFunction := TestFiles;
        end;
      'x':
        begin
          Command := cmdExtract_X;
          CommandFunction := ExtractFiles;
        end;
    else
      begin
        WriteLn(Format('Invalid InputCommand "%s"', [InputCommand]));
        Exit;
      end;
    end;

    Result := CommandFunction(VaultName, Files);

    if Result <> 0 then
      WriteLn(Format('Error: %s', [Error2Str(Result, '')]))
    else
      WriteLn('Done');

  finally
    FreeAndNil(Files);
  end;
end;

end.






