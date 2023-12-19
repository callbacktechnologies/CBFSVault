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

program vaultexplorer;

uses
  Forms,
  aboutf in 'aboutf.pas'   {FormAboutf},
  exportf in 'exportf.pas'   {FormExportf},
  filepropsf in 'filepropsf.pas'   {FormFilepropsf},
  importf in 'importf.pas'   {FormImportf},
  importfilef in 'importfilef.pas'   {FormImportfilef},
  linkf in 'linkf.pas'   {FormLinkf},
  openvaultf in 'openvaultf.pas'   {FormOpenvaultf},
  passwordf in 'passwordf.pas'   {FormPasswordf},
  progressf in 'progressf.pas'   {FormProgressf},
  strres in 'strres.pas' ,
  vaultencryptionf in 'vaultencryptionf.pas'   {FormVaultencryptionf},
  vaultpropsf in 'vaultpropsf.pas'   {FormVaultpropsf},
  vaultselectfilef in 'vaultselectfilef.pas'   {FormVaultselectfilef},
  vaultselectfolderf in 'vaultselectfolderf.pas'   {FormVaultselectfolderf},
  wizardf in 'wizardf.pas'   {FormWizardf},
  vaultexplorerf in 'vaultexplorerf.pas' {FormVaultexplorer};

begin
  Application.Initialize;

  Application.CreateForm(TFormVaultexplorer, FormVaultexplorer);
  Application.CreateForm(TFormAbout, FormAbout);

  Application.CreateForm(TFormExport, FormExport);

  Application.CreateForm(TFormFileprops, FormFileprops);

  Application.CreateForm(TFormImport, FormImport);

  Application.CreateForm(TFormImportfile, FormImportfile);

  Application.CreateForm(TFormLink, FormLink);

  Application.CreateForm(TFormOpenvault, FormOpenvault);

  Application.CreateForm(TFormPassword, FormPassword);

  Application.CreateForm(TFormProgress, FormProgress);



  Application.CreateForm(TFormVaultencryption, FormVaultencryption);

  Application.CreateForm(TFormVaultprops, FormVaultprops);

  Application.CreateForm(TFormVaultselectfile, FormVaultselectfile);

  Application.CreateForm(TFormVaultselectfolder, FormVaultselectfolder);

  Application.CreateForm(TFormWizard, FormWizard);

  Application.Run;
end.


         
