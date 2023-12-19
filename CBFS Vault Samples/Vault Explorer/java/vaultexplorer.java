/*
 * CBFS Vault 2022 Java Edition - Sample Project
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
 */

import java.io.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.tree.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.nio.file.Path;
import java.nio.file.Paths;

import cbfsvault.*;

public class vaultexplorer implements CbvaultEventListener, ActionListener, TreeSelectionListener, ListCellRenderer, ListSelectionListener {

  private Cbvault Vault = null;

  private JFrame frameMain;

  private JPanel panelRoot;
  private JTree treeFolders;
  private JList listFiles;
  private JProgressBar progressBar;
  private JPanel panelMain;

  private JMenuItem menuItemVaultCreate;
  private JMenuItem menuItemVaultOpen;
  private JMenuItem menuItemVaultClose;
  private JMenuItem menuItemVaultCompact;
  private JMenuItem menuItemVaultCheckRepair;
  private JMenuItem menuItemVaultExit;
  private JMenuItem menuItemCreateFolder;
  private JMenuItem menuItemDeleteFolder;
  private JMenuItem menuItemRenameFolder;
  private JMenuItem menuItemImportFile;
  private JMenuItem menuItemExportFile;
  private JMenuItem menuItemRenameFile;
  private JMenuItem menuItemDeleteFile;

  public void actionPerformed(ActionEvent e)
  {
    if (e.getSource() == menuItemVaultCreate)
      onVaultCreateClick();
    else if (e.getSource() == menuItemVaultOpen)
      onVaultOpenClick();
    else if (e.getSource() == menuItemVaultClose)
      onVaultCloseClick();
    else if (e.getSource() == menuItemVaultCompact)
      onVaultCompactClick();
    else if (e.getSource() == menuItemVaultCheckRepair)
      onVaultCheckRepairClick();
    else if (e.getSource() == menuItemVaultExit)
      onVaultExitClick();
    else if (e.getSource() == menuItemCreateFolder)
      onCreateFolderClick();
    else if (e.getSource() == menuItemDeleteFolder)
      onDeleteFolderClick();
    else if (e.getSource() == menuItemRenameFolder)
      onRenameFolderClick();
    else if (e.getSource() == menuItemImportFile)
      onImportFileClick();
    else if (e.getSource() == menuItemExportFile)
      onExportFileClick();
    else if (e.getSource() == menuItemRenameFile)
      onRenameFileClick();
    else if (e.getSource() == menuItemDeleteFile)
      onDeleteFileClick();
  }

  public void valueChanged(TreeSelectionEvent e) {

    setFolder((DefaultMutableTreeNode)e.getPath().getLastPathComponent());
  }

  public void valueChanged(ListSelectionEvent e) {

    updateMenus();
  }

  protected class ListItem
  {
    String Item;
    boolean Dir;

    ListItem(String Item, boolean Dir)
    {
      this.Item = Item;
      this.Dir = Dir;
    }

    @Override
    public String toString() {
      return Item;
    }
  }

  public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected, boolean cellHasFocus) {

    DefaultListCellRenderer defaultRenderer = new DefaultListCellRenderer();
    JLabel label = (JLabel) defaultRenderer.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);

    ListItem Item = (ListItem)value;
    DefaultTreeCellRenderer renderer = (DefaultTreeCellRenderer) treeFolders.getCellRenderer();

    if (Item.Dir)
      label.setIcon(renderer.getDefaultOpenIcon());
    else
      label.setIcon(renderer.getDefaultLeafIcon());

    return label;
  }

  public void initComponent(JFrame frame)
  {
    frameMain = frame;

    frameMain.addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
        onVaultCloseClick();
      }
    });

    JMenuBar menuMain = new JMenuBar();
    frame.setJMenuBar(menuMain);

    treeFolders.setModel(new DefaultTreeModel(null));
    listFiles.setModel(new DefaultListModel());

    treeFolders.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
    treeFolders.putClientProperty("JTree.lineStyle", "Angled");
    treeFolders.setShowsRootHandles(true);

    DefaultTreeCellRenderer renderer = (DefaultTreeCellRenderer) treeFolders.getCellRenderer();
    renderer.setClosedIcon(renderer.getDefaultOpenIcon());
    renderer.setOpenIcon(renderer.getDefaultOpenIcon());
    renderer.setLeafIcon(renderer.getDefaultOpenIcon());

    listFiles.setCellRenderer(this);

    treeFolders.addTreeSelectionListener(this);
    listFiles.addListSelectionListener(this);

    //Vault category
    JMenu vaultMenu = new JMenu("Vault");
    menuMain.add(vaultMenu);
    {
      menuItemVaultCreate = new JMenuItem("Create");
      menuItemVaultCreate.addActionListener(this);
      vaultMenu.add(menuItemVaultCreate);

      menuItemVaultOpen = new JMenuItem("Open");
      menuItemVaultOpen.addActionListener(this);
      vaultMenu.add(menuItemVaultOpen);

      menuItemVaultClose = new JMenuItem("Close");
      menuItemVaultClose.addActionListener(this);
      vaultMenu.add(menuItemVaultClose);

      menuItemVaultCompact = new JMenuItem("Compact");
      menuItemVaultCompact.addActionListener(this);
      vaultMenu.add(menuItemVaultCompact);

      menuItemVaultCheckRepair = new JMenuItem("Check & Repair");
      menuItemVaultCheckRepair.addActionListener(this);
      vaultMenu.add(menuItemVaultCheckRepair);

      vaultMenu.addSeparator();

      menuItemVaultExit = new JMenuItem("Exit");
      menuItemVaultExit.addActionListener(this);
      vaultMenu.add(menuItemVaultExit);
    }

    //folderAndFiles category
    JMenu folderAndFilesMenu = new JMenu("Folder And Files");
    menuMain.add(folderAndFilesMenu);
    {
      menuItemCreateFolder = new JMenuItem("Create Folder");
      menuItemCreateFolder.addActionListener(this);
      folderAndFilesMenu.add(menuItemCreateFolder);

      menuItemDeleteFolder = new JMenuItem("Delete Folder");
      menuItemDeleteFolder.addActionListener(this);
      folderAndFilesMenu.add(menuItemDeleteFolder);

      menuItemRenameFolder = new JMenuItem("Rename Folder");
      menuItemRenameFolder.addActionListener(this);
      folderAndFilesMenu.add(menuItemRenameFolder);

      folderAndFilesMenu.addSeparator();

      menuItemImportFile = new JMenuItem("Import File");
      menuItemImportFile.addActionListener(this);
      folderAndFilesMenu.add(menuItemImportFile);

      menuItemExportFile = new JMenuItem("Export File");
      menuItemExportFile.addActionListener(this);
      folderAndFilesMenu.add(menuItemExportFile);

      menuItemRenameFile = new JMenuItem("Rename File");
      menuItemRenameFile.addActionListener(this);
      folderAndFilesMenu.add(menuItemRenameFile);

      menuItemDeleteFile = new JMenuItem("Delete File");
      menuItemDeleteFile.addActionListener(this);
      folderAndFilesMenu.add(menuItemDeleteFile);
    }

    updateMenus();
  }

  private boolean getOpened()
  {
    if (Vault != null)
      return Vault.isActive();
    return false;
  }

  private void setOpened(boolean value)
  {
    try
    {
      if (!value)
      {
        if (Vault.isActive())
          Vault.closeVault();
        Vault = null;

        ((DefaultListModel)listFiles.getModel()).removeAllElements();

        DefaultTreeModel model = (DefaultTreeModel) treeFolders.getModel();
        ((DefaultMutableTreeNode) model.getRoot()).removeAllChildren();
        model.reload();
        model.setRoot(null);
      }
      else
      {
        panelMain.setVisible(true);
      }
      updateMenus();
    }
    catch (Exception ex)
    {

    }
  }

  private void updateMenus()
  {
    boolean SelectionExists = (treeFolders.getLastSelectedPathComponent()) != null;

    menuItemVaultClose.setEnabled(getOpened());
    menuItemVaultCheckRepair.setEnabled(getOpened());
    menuItemCreateFolder.setEnabled(getOpened());
    menuItemVaultCompact.setEnabled(getOpened());

    menuItemDeleteFolder.setEnabled(getOpened() && SelectionExists && (treeFolders.getSelectionPath().getPath().length > 1));//not root
    menuItemRenameFolder.setEnabled(menuItemDeleteFolder.isEnabled());

    menuItemImportFile.setEnabled(getOpened() && SelectionExists);

    SelectionExists = listFiles.getSelectedIndex() != -1;

    menuItemDeleteFile.setEnabled(getOpened() && SelectionExists);
    menuItemExportFile.setEnabled(menuItemDeleteFile.isEnabled());
    menuItemRenameFile.setEnabled(menuItemDeleteFile.isEnabled());
  }

  private boolean checkOpened()
  {
    if (getOpened())
    {
      if (JOptionPane.showConfirmDialog(frameMain,
              "Do you want to close currently opened vault?",
              "Close vault?",
              JOptionPane.YES_NO_OPTION) == JOptionPane.NO_OPTION)
        return false;
      else
      {
        setOpened(false);
        return true;
      }
    }
    else
      return true;
  }

  private void addSubFolders(String BasePath, DefaultMutableTreeNode BaseNode)
  {
    DefaultMutableTreeNode CurrentNode = null;

    String mask = "";
    if (BasePath.equals("\\"))
      mask = "\\*";
    else
      mask = BasePath + "\\*";

    try
    {
      long handle = Vault.findFirst(mask, Constants.VAULT_FATTR_ANY_FILE, 0);
      boolean find = handle != -1;
      if (find)
      {
        while (find)
        {
          int attr = Vault.getSearchResultAttributes(handle);
          if ((attr & Constants.VAULT_FATTR_DIRECTORY) == Constants.VAULT_FATTR_DIRECTORY)
          {
            CurrentNode = new DefaultMutableTreeNode(Vault.getSearchResultName(handle));
            BaseNode.add(CurrentNode);
            addSubFolders(Vault.getSearchResultFullName(handle), CurrentNode);
          }
          find = Vault.findNext(handle);
        }
        Vault.findClose(handle);
      }
    }
    catch(Exception ex)
    {

    }
  }

  private void rebuildFolders()
  {
    try
    {
      DefaultMutableTreeNode RootNode = new DefaultMutableTreeNode("\\");
      DefaultTreeModel model = (DefaultTreeModel) treeFolders.getModel();
      model.setRoot(RootNode);

      addSubFolders("\\", RootNode);

      treeFolders.setSelectionPath(new TreePath(RootNode));
    }
    finally
    {
    }
  }

  private String getNodeFullPath(DefaultMutableTreeNode Node)
  {
    String BasePath = "";

    if (Node == null)
      return BasePath;

    Object[] UserPath = Node.getUserObjectPath();
    for (Object x: UserPath)
    {
      if (BasePath.length() > 0)
        BasePath += "\\";
      BasePath += (String)x;
    }

    return BasePath;
  }

  private void setFolder(DefaultMutableTreeNode Node)
  {
    ((DefaultListModel)listFiles.getModel()).removeAllElements();

    if (Node != null)
    {
      String BasePath = getNodeFullPath(Node);

      try
      {
        String mask = "";
        if (BasePath.equals("\\"))
          mask = "\\*";
        else
          mask = BasePath + "\\*";

        long handle = Vault.findFirst(mask, Constants.VAULT_FATTR_ANY_FILE, 0);
        boolean b = handle != 0;
        if (b)
        {
          while (b)
          {
            int attr = Vault.getSearchResultAttributes(handle);

            ListItem Item = new ListItem(Vault.getSearchResultName(handle),
                    (attr & Constants.VAULT_FATTR_DIRECTORY) == Constants.VAULT_FATTR_DIRECTORY);
            ((DefaultListModel)listFiles.getModel()).addElement(Item);

            b = Vault.findNext(handle);
          }
          Vault.findClose(handle);
        }
      }
      catch (Exception ex)
      {

      }
    }
    updateMenus();
  }

  private void onVaultCreateClick()
  {
    if (!checkOpened())
      return;

    JFileChooser Chooser = new JFileChooser();
    filenameextensionfilter Filter = new filenameextensionfilter(
            "Vault files (*.svlt)", "svlt");
    Chooser.setFileFilter(Filter);

    if (Chooser.showSaveDialog(frameMain) == JFileChooser.APPROVE_OPTION)
    {
      try
      {
        Vault = new Cbvault();
        Vault.addCbvaultEventListener(this);

        Vault.setVaultFile(Chooser.getSelectedFile().getAbsolutePath());
        Vault.setLogo("Vault Explorer sample vault");
        Vault.setPageSize(512);
        Vault.setPathSeparator('\\');
        Vault.setUseAccessTime(false);
        Vault.openVault(Constants.VAULT_OM_CREATE_ALWAYS, Constants.VAULT_JM_NONE);
        setOpened(true);
        rebuildFolders();
      }
      catch (Exception ex)
      {
        JOptionPane.showMessageDialog(frameMain,ex.getMessage(),"Error",JOptionPane.ERROR_MESSAGE);
      }
    }
  }

  private void onVaultOpenClick()
  {
    if (checkOpened() == false)
      return;

    JFileChooser Chooser = new JFileChooser();
    filenameextensionfilter Filter = new filenameextensionfilter(
            "Vault files (*.svlt)", "svlt");
    Chooser.setFileFilter(Filter);

    if (Chooser.showOpenDialog(frameMain) == JFileChooser.APPROVE_OPTION)
    {
      try
      {
        Vault = new Cbvault();
        Vault.addCbvaultEventListener(this);

        Vault.setVaultFile(Chooser.getSelectedFile().getAbsolutePath());
        Vault.setPathSeparator('\\');
        Vault.setUseAccessTime(false);

        Vault.openVault(Constants.VAULT_OM_OPEN_EXISTING, Constants.VAULT_JM_NONE);
        setOpened(true);
        rebuildFolders();
      }
      catch (Exception ex)
      {
        JOptionPane.showMessageDialog(frameMain,ex.getMessage(),"Error",JOptionPane.ERROR_MESSAGE);
      }
    }
  }

  private void onVaultCloseClick()
  {
    if (checkOpened() == false)
      return;
    updateMenus();
  }

  private void onVaultCompactClick()
  {
    try
    {
      Vault.compactVault();
    }
    catch (Exception ex)
    {
      JOptionPane.showMessageDialog(frameMain,ex.getMessage(),"Error",JOptionPane.ERROR_MESSAGE);
    }
  }

  private void onVaultCheckRepairClick()
  {
    try
    {
      Vault.closeVault();
      Vault.checkAndRepair(0);
      Vault.openVault(Constants.VAULT_OM_OPEN_EXISTING, Constants.VAULT_JM_NONE);
    }
    catch (Exception ex)
    {
      JOptionPane.showMessageDialog(frameMain,ex.getMessage(),"Error",JOptionPane.ERROR_MESSAGE);
    }
  }

  private void onVaultExitClick()
  {
    onVaultCloseClick();
    frameMain.dispatchEvent(new WindowEvent(frameMain, WindowEvent.WINDOW_CLOSING));
  }

  private void onCreateFolderClick()
  {
    inputform inputBox = new inputform("Folder name", "Please enter the name for the new folder");

    String FolderName = "";
    String FullName = "";
    DefaultMutableTreeNode ParentNode = (DefaultMutableTreeNode)treeFolders.getLastSelectedPathComponent();

    while (true)
    {
      inputBox.setVisible(true);
      if (inputBox.strValue.length() == 0)
        return;
      FolderName = inputBox.strValue;

      if (ParentNode != null)
        FullName = getNodeFullPath(ParentNode) + "\\" + FolderName;
      else
        FullName = "\\" + FolderName;

      try
      {
        if (Vault.fileExists(FullName))
        {
          JOptionPane.showMessageDialog(frameMain,"The folder with specified name already exists","Error",JOptionPane.ERROR_MESSAGE);
          continue;
        }

        try
        {
          Vault.createDirectory(FullName, true);
        }
        catch (Exception Ex)
        {
          JOptionPane.showMessageDialog(frameMain,Ex.getMessage(),"Error",JOptionPane.ERROR_MESSAGE);
          continue;
        }

        if (ParentNode == null)
          ParentNode = (DefaultMutableTreeNode)treeFolders.getModel().getRoot();
        DefaultMutableTreeNode Current = new DefaultMutableTreeNode(FolderName);

        ParentNode.add(Current);


        TreeNode[] ParPath = ParentNode.getPath();
        TreeNode[] CurPath = new TreeNode[ParPath.length + 1];

        System.arraycopy(ParPath, 0, CurPath, 0, ParPath.length);
        CurPath[ParPath.length] = Current;
        treeFolders.getSelectionModel().setSelectionPath(new TreePath(CurPath));

        setFolder(Current);
        break;
      }
      catch (Exception E)
      {
        JOptionPane.showMessageDialog(frameMain,E.getMessage(),"Error",JOptionPane.ERROR_MESSAGE);
        return;
      }
    }
  }

  private void onDeleteFolderClick()
  {
    DefaultMutableTreeNode Node = (DefaultMutableTreeNode)treeFolders.getLastSelectedPathComponent();
    if (Node != null)
    {
      String FullName = getNodeFullPath(Node);

      if (JOptionPane.showConfirmDialog(frameMain,
              "Do you want to delete " + Node.getUserObject()+ " and all it's contents?",
              "Confirm deletion",
              JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION)
      {
        try
        {
          Vault.deleteFile(FullName);
          ((DefaultMutableTreeNode)Node.getParent()).remove(Node);
          treeFolders.invalidate();
        }
        catch (Exception E)
        {}
      }
    }
  }

  private void onRenameFolderClick()
  {
    DefaultMutableTreeNode Node = (DefaultMutableTreeNode)treeFolders.getLastSelectedPathComponent();
    String oldFolderName = (String)Node.getUserObject();

    inputform inputBox = new inputform("Folder name", "Please enter the name for the new folder", oldFolderName);

    String FolderName = "";
    String oldFullName = "";
    String FullName = "";

    DefaultMutableTreeNode ParentNode = (DefaultMutableTreeNode)Node.getParent();

    while (true)
    {
      inputBox.setVisible(true);
      if (inputBox.strValue.length() == 0)
        return;
      FolderName = inputBox.strValue;

      if (ParentNode != null)
      {
        FullName = getNodeFullPath(ParentNode) + "\\" + FolderName;
        oldFullName = getNodeFullPath(ParentNode) + "\\" + oldFolderName;
      }
      else
      {
        FullName = "\\" + FolderName;
        oldFullName = "\\" + oldFolderName;
      }

      try
      {
        if (Vault.fileExists(FullName))
        {
          JOptionPane.showMessageDialog(frameMain,"The folder with specified name already exists","Error",JOptionPane.ERROR_MESSAGE);
          continue;
        }

        try
        {
          Vault.moveFile(oldFullName, FullName, false);
        }
        catch (Exception Ex)
        {
          JOptionPane.showMessageDialog(frameMain,Ex.getMessage(),"Error",JOptionPane.ERROR_MESSAGE);
          continue;
        }

        Node.setUserObject(FolderName);
        treeFolders.invalidate();
        break;
      }
      catch (Exception E)
      {
        JOptionPane.showMessageDialog(frameMain,E.getMessage(),"Error",JOptionPane.ERROR_MESSAGE);
        return;
      }
    }
  }

  private void onImportFileClick()
  {
    JFileChooser Chooser = new JFileChooser();

    if (Chooser.showOpenDialog(frameMain) == JFileChooser.APPROVE_OPTION)
    {
      String FileName = Chooser.getSelectedFile().getName();
      String FullName = null;

      DefaultMutableTreeNode ParentNode = (DefaultMutableTreeNode)treeFolders.getLastSelectedPathComponent();

      inputform inputBox = new inputform("File name", "Please enter the name for the new file", FileName);

      while (true)
      {
        inputBox.setVisible(true);
        if (inputBox.strValue.length() == 0)
          return;

        if (ParentNode != null)
          FullName = getNodeFullPath(ParentNode) + "\\" + FileName;
        else
          FullName = "\\" + FileName;

        try
        {
          if (Vault.fileExists(FullName))
          {
            JOptionPane.showMessageDialog(frameMain,"The file or folder with specified name already exists","Error",JOptionPane.ERROR_MESSAGE);
            continue;
          }
        }
        catch (Exception Ex){ return;}
        break;
      }

      File TheFile = new File(Chooser.getSelectedFile().getAbsolutePath());
      try
      {
        FileInputStream TheFileStream = new FileInputStream(TheFile);

        CBFSVaultStream Stream = Vault.openFileEx(
                FullName, Constants.VAULT_OM_CREATE_NEW,
                false, true, true, true,
                Constants.VAULT_EM_NONE, "",
                Constants.VAULT_CM_NONE, 0, 0);

        byte[] Buffer = new byte[1024 * 1024];
        long TotalRead = 0;
        int BytesRead;
        while (TotalRead < TheFile.length())
        {
          BytesRead = TheFileStream.read(Buffer);
          if (BytesRead != 0)
          {
            byte[] WBuffer = Buffer;
            if (BytesRead != Buffer.length)
            {
              WBuffer = new byte[BytesRead];
              System.arraycopy(Buffer, 0, WBuffer, 0, BytesRead);
            }
            BytesRead = Stream.write(WBuffer);
            TotalRead += BytesRead;
          }
        }

        Stream.close();
        TheFileStream.close();

        //Vault.setFileEncryption(FullName, Constants.VAULT_EM_CUSTOM256_DEFAULT, "", "password");
        //int Encryption = Vault.getFileEncryption(FullName);
        //if (Encryption == Constants.VAULT_EM_NONE)
        //  JOptionPane.showMessageDialog(frameMain,"no encryption","Error",JOptionPane.ERROR_MESSAGE);

        ListItem Item = new ListItem(FileName,false);
        ((DefaultListModel)listFiles.getModel()).addElement(Item);
        listFiles.invalidate();
      }
      catch (Exception ex)
      {

      }
    }
  }

  private void onExportFileClick()
  {
    String FileName = listFiles.getSelectedValue().toString();

    JFileChooser Chooser = new JFileChooser();
    Chooser.setSelectedFile(new File(FileName));

    if (Chooser.showOpenDialog(frameMain) == JFileChooser.APPROVE_OPTION)
    {
      DefaultMutableTreeNode ParentNode = (DefaultMutableTreeNode)treeFolders.getLastSelectedPathComponent();
      String FullName = null;

      if (ParentNode != null)
        FullName = getNodeFullPath(ParentNode) + "\\" + FileName;
      else
        FullName = "\\" + FileName;

      File TheFile = new File(Chooser.getSelectedFile().getAbsolutePath());
      try
      {
        FileOutputStream TheFileStream = new FileOutputStream(TheFile);
        CBFSVaultStream Stream = Vault.openFile(
                FullName, Constants.VAULT_OM_OPEN_ALWAYS,
                true, false, "");

        long TotalRead = 0;
        int BytesRead;
        while (TotalRead < Stream.getLength())
        {
          byte[] Buffer = Stream.read(1024*1024);
          BytesRead = Buffer.length;
          TheFileStream.write(Buffer, 0, BytesRead);
          TotalRead += BytesRead;
        }

        Stream.close();
        TheFileStream.close();
      }
      catch(Exception ex)
      {

      }
    }
  }

  private void onRenameFileClick()
  {
    DefaultMutableTreeNode ParentNode = (DefaultMutableTreeNode)treeFolders.getLastSelectedPathComponent();
    String oldFileName = listFiles.getSelectedValue().toString();
    String oldFullName = getNodeFullPath(ParentNode) + "\\" + oldFileName;
    String FileName;
    String FullName;

    inputform inputBox = new inputform("File name", "Please enter new name for the file", oldFileName);

    try
    {
      while (true)
      {
        inputBox.setVisible(true);
        if (inputBox.strValue.length() == 0)
          return;
        FileName = inputBox.strValue;

        if (ParentNode != null)
          FullName = getNodeFullPath(ParentNode) + "\\" + FileName;
        else
          FullName = "\\" + FileName;

        if (Vault.fileExists(FullName))
        {
          JOptionPane.showMessageDialog(frameMain,"The file or folder with specified name already exists","Error",JOptionPane.ERROR_MESSAGE);
          continue;
        }
        break;
      }

      Vault.moveFile(oldFullName, FullName, false);
      ((ListItem)listFiles.getSelectedValue()).Item = FileName;
      listFiles.invalidate();
    }
    catch (Exception e)
    {}
  }

  private void onDeleteFileClick()
  {
    if (listFiles.getSelectedValue() != null)
    {
      String FullName = getNodeFullPath((DefaultMutableTreeNode)treeFolders.getLastSelectedPathComponent()) +
              "\\" + listFiles.getSelectedValue().toString();

      if (JOptionPane.showConfirmDialog(frameMain,
              String.format("Do you want to delete %s?", FullName),
              "Confirm deletion",
              JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION)
      {
        try {
          Vault.deleteFile(FullName);
          ((DefaultListModel) listFiles.getModel()).remove(listFiles.getSelectedIndex());
        }
        catch (Exception e)
        {
        }
      }
    }
  }

  public void filePasswordNeeded(CbvaultFilePasswordNeededEvent cbvaultFilePasswordNeededEvent) {

  }

  public void dataCompress(CbvaultDataCompressEvent cbvaultDataCompressEvent) {

  }

  public void dataDecompress(CbvaultDataDecompressEvent cbvaultDataDecompressEvent) {

  }

  public void dataDecrypt(CbvaultDataDecryptEvent cbvaultDataDecryptEvent) {

    for (int i = 0; i < cbvaultDataDecryptEvent.dataSize; i++)
      cbvaultDataDecryptEvent.data.put(i,
              (byte)(cbvaultDataDecryptEvent.data.get(i) ^ cbvaultDataDecryptEvent.key.get(i % cbvaultDataDecryptEvent.keyLength)));
    cbvaultDataDecryptEvent.resultCode = 0;
  }

  public void dataEncrypt(CbvaultDataEncryptEvent cbvaultDataEncryptEvent) {

    for (int i = 0; i < cbvaultDataEncryptEvent.dataSize; i++)
      cbvaultDataEncryptEvent.data.put(i,
              (byte)(cbvaultDataEncryptEvent.data.get(i) ^ cbvaultDataEncryptEvent.key.get(i % cbvaultDataEncryptEvent.keyLength)));
    cbvaultDataEncryptEvent.resultCode = 0;
  }

  public void error(CbvaultErrorEvent cbvaultErrorEvent) {

  }

  public void fileAfterCopy(CbvaultFileAfterCopyEvent cbvaultFileAfterCopyEvent) {

  }

  public void fileBeforeCopy(CbvaultFileBeforeCopyEvent cbvaultFileBeforeCopyEvent) {

  }

  public void hashCalculate(CbvaultHashCalculateEvent cbvaultHashCalculateEvent) {

    for (int i = 0; i < cbvaultHashCalculateEvent.hashSize; i++)
      cbvaultHashCalculateEvent.hash.put(i, (byte) 0);
    cbvaultHashCalculateEvent.resultCode = 0;
  }

  public void keyDerive(CbvaultKeyDeriveEvent cbvaultKeyDeriveEvent) {

  }

  public void progress(CbvaultProgressEvent cbvaultProgressEvent) {

    progressBar.setMinimum(0);
    progressBar.setMaximum(cbvaultProgressEvent.total);
    progressBar.setValue(cbvaultProgressEvent.progress);
    progressBar.invalidate();
  }

  public void vaultClose(CbvaultVaultCloseEvent cbvaultVaultCloseEvent) {

  }

  public void vaultDelete(CbvaultVaultDeleteEvent cbvaultVaultDeleteEvent) {

  }

  public void vaultFlush(CbvaultVaultFlushEvent cbvaultVaultFlushEvent) {

  }

  public void vaultGetParentSize(CbvaultVaultGetParentSizeEvent cbvaultVaultGetParentSizeEvent) {

  }

  public void vaultGetSize(CbvaultVaultGetSizeEvent cbvaultVaultGetSizeEvent) {

  }

  public void vaultOpen(CbvaultVaultOpenEvent cbvaultVaultOpenEvent) {

  }

  public void vaultRead(CbvaultVaultReadEvent cbvaultVaultReadEvent) {

  }

  public void vaultSetSize(CbvaultVaultSetSizeEvent cbvaultVaultSetSizeEvent) {

  }

  public void vaultWrite(CbvaultVaultWriteEvent cbvaultVaultWriteEvent) {

  }

  private boolean isDriveLetter(String path) {
    if (path == null || path.isEmpty() || path.length() < 2)
      return false;

    char c = path.charAt(0);
    if ((((int)c >= (int) 'A' && (int)c <= (int) 'Z') ||
            ((int)c >= (int) 'a' && (int)c <= (int) 'z')) &&
            (path.charAt(1) == ':'))
      return true;
    else
      return false;
  }

  private String ConvertRelativePathToAbsolute(String path) {
    String res = null;
    if (path != null && !path.isEmpty()) {
      res = path;

      // Linux-specific case of using a home directory
      if (path.equals("~") || path.startsWith("~/"))
      {
        String homeDir = System.getProperty("user.home");
        if (path.equals("~"))
          return homeDir;
        else
          return homeDir + path.substring(1);
      }
      else
      if (!isDriveLetter(path)) {
        Path fullPath = Paths.get(path).toAbsolutePath().normalize();
        res = fullPath.toString();

        File file = new File(res);

        if (res.startsWith("\\\\") && !file.exists()) {
          JOptionPane.showMessageDialog(frameMain,
                  "The network folder '" + res + "' does not exist.", "Error", JOptionPane.ERROR_MESSAGE);
        } else if (!file.exists()) {
          JOptionPane.showMessageDialog(frameMain,
                  "The path '" + res + "' does not exist.", "Error", JOptionPane.ERROR_MESSAGE);
        }
      }
    }
    return res;
  }

  public static void main(String[] args) {
    JFrame frame = new JFrame("Vault Explorer");
    vaultexplorer vaultexplorerDemo = new vaultexplorer();

    frame.setContentPane(vaultexplorerDemo.panelRoot);
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

    vaultexplorerDemo.initComponent(frame);

    frame.pack();
    frame.setVisible(true);
  }

}






