unit strres;

interface

uses
  Windows;

const
  REG_ROOTKEY = HKEY_CURRENT_USER;

resourcestring
  REG_PATH = '\Software\CBFS Vault\Vault Explorer';
  REG_VAULTS = '\Vaults';

  RegistrationKeyWarning = 'CBFS Vault is working in the evaluation mode.'#13#10 +
    'Any entered password for encryption will be replaced with empty password.'#13#10 +
    'If you have valid registration key, then place it in the file:'#13#10'%s';

  PasswordsNotEqual = 'The entered passwords did not match.';

  VaultAlreadyOpened = 'Vault "%s" already opened.';
  InvalidVaultFile = 'Vault Error: Invalid vault file "%s".';

  VaultCompacted = 'Vault compacted successfully: %s saved';

  FolderDeleteConfirmation = 'Are you sure you want to delete the folder "%s" and all its contents?';
  FileDeleteConfirmation = 'Are you sure you want to delete the file "%s"?';
  MultipleFileDeleteConfirmation = 'Are you sure you want to delete these %d items?';

  CannotMoveFolderToSubfolder = 'Cannot move "%s": The destination folder is a subfolder of the source folder.';
  CannotMoveFolderToSelf = 'Cannot move "%s": The destination folder is the same as the source folder.';
  CannotMoveFileToSelf = 'Cannot move "%s": The source and destination file names are the same.';

  ReplaceExistingFile = 'File "%s" already exists.'#13#10#13#10'Would you like to replace the existing file'#13#10 +
    '   Size: %s, Modified: %s'#13#10'With this one?'#13#10'   Size: %s, Modified: %s';

  CopyToFolder_Caption = 'Copy Items';
  CopyToFolder_ButtonCaption = 'Copy';
  CopyToFolder_Title = 'Select the place where you want to copy "%s". Then click the Copy button.';
  CopyToFolder_MultipleTitle = 'Select the place where you want to copy these %d items. Then click the Copy button.';

  MoveToFolder_Caption = 'Move Items';
  MoveToFolder_ButtonCaption = 'Move';
  MoveToFolder_Title = 'Select the place where you want to move "%s". Then click the Move button.';
  MoveToFolder_MultipleTitle = 'Select the place where you want to move these %d items. Then click the Move button.';

  VaultCheckAndRepairOK = 'Vault file passed OK the "Check and Repair" procedure.';

  // Form Import File
  IMPORT_FILE_SelectMessage = 'Select properties for creating file:'#13#10'  %s';

  PASSWORD_Prompt = 'Password required to access %s %s';

  FILE_PROPS_SelectMessage = 'Select advanced properties for file:'#13#10'  %s';

  VAULT_PROPS_SelectMessage = 'Select advanced properties for vault:'#13#10'  %s';

implementation

end.
