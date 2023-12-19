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
import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

import cbfsvault.*;

public class vaultmgr {

    enum Commands
    {
        Add,
        Delete,
        Extract,
        List,
        Test,
        eXtractWithPath
    }

    final static private String AllCommands = "adeltx";
    final static private String AllOptions = "cdfgloprqsyuw-";
    final static private String AllOptionsWithParameters = "cfgopsw";

    final static private int BUFFER_SIZE = 1024*1024;

    final static private int ANSWER_UNKNOWN = 0;
    final static private int ANSWER_YES = 1;
    final static private int ANSWER_NO =  2;
    final static private int ANSWER_ALL = 3;

    static private Commands TheCommand;

    static private boolean OptionAlwaysYes = false;
    static private int     OptionCompressionLevel = -1;
    static private boolean OptionDeleteAfterArchiving = false;
    static private int     OptionFixedVaultSize = 0;
    static private boolean OptionLowerCase = false;
    static private String  OptionOutputPath = "";
    static private boolean OptionOverwriteFiles = false;
    static private boolean OptionOverwriteVault = false;
    static private int     OptionPageSize = 0;
    static private String  OptionPassword = "";
    static private boolean OptionRecursive = false;
    static private boolean OptionUpperCase = false;

    static private boolean _OptionOverwriteFilesSet = false;
    static private String  _OutputPath = "";

    //-----------------------------------------------------------------------------
    static private void Usage()
    {
        System.out.println("Usage: vaultmgr <command> [-<switch 1> ... -<switch N>] <vault> [<files>...]\n");
        System.out.println("<Commands>");
        System.out.println("  a             Add files to vault");
        System.out.println("  d             Delete files from vault");
        System.out.println("  e             Extract files from vault");
        System.out.println("  l             List contents of vault");
        System.out.println("  t             Test integrity of vault");
        System.out.println("  x             eXtract files with full pathname");

        System.out.println("<Switches>");
        System.out.println("  -             Stop switches scanning");

        System.out.println("  l             Convert names to lower case");
        System.out.println("  u             Convert names to upper case");
        System.out.println("  d             Delete files after archiving");
        System.out.println("  c<0-9>        set Compression level (0 - default)");
        System.out.println("  e             Exclude paths from names");
        System.out.println("  f<size>       set the Fixed size of the vault in MB");
        System.out.println("  g<page size>  set size of the paGe for new vault (4096 - default)");
        System.out.println("  p<password>   set Password");
        System.out.println("  o<path>       set Output directory");
        System.out.println("  r             Recurse subdirectories");
        System.out.println("  s+            overwrite existing Vault");
        System.out.println("  s-            do not overwrite Vault (default)");
        System.out.println("  y             Assume Yes on all queries");
        System.out.println("  w+            overWrite existing files");
        System.out.println("  w-            do not overWrite existing files (default)");
    }

    final static private char DriveDelimiter = ':';

    //-----------------------------------------------------------------------------

    static private boolean isNullOrEmpty(String value)
    {
        return (value == null || value.length() == 0);
    }

    static private String OSPathToVaultPath(String OSPath)
    {
        String Result = OSPath.replace('/', '\\');

        if ((OSPath.length() > 2) && (OSPath.charAt(1) == DriveDelimiter))
            Result = Result.substring(2);

        while (true) {
            if (Result.startsWith(".\\"))
                Result = Result.substring(2);
            else if (Result.startsWith("..\\"))
                Result = Result.substring(3);
            else
                break;
        }

        if (!Result.startsWith("\\"))
            Result = "\\" + Result;
        if (OptionUpperCase)
            Result = Result.toUpperCase();
        if (OptionLowerCase)
            Result = Result.toLowerCase();
        return Result;
    }

    //-----------------------------------------------------------------------------
    static private String ExtractFilePathEx(String FileName, char PathDelimiter)
    {
        int LastPos = FileName.lastIndexOf(PathDelimiter);
        if (LastPos != -1)
            return FileName.substring(0, LastPos);
        else
            return "";
    }

    //-----------------------------------------------------------------------------
    static private String ExtractFileNameEx(String FileName, char PathDelimiter)
    {
        int LastPos = FileName.lastIndexOf(PathDelimiter);
        if (LastPos == -1)
            return FileName;
        else
            return FileName.substring(LastPos+1);
    }

    //-----------------------------------------------------------------------------
    static private void SplitPathWithMaskEx(String PathWithMask, String[] args, char PathDelimiter)
    {
        if ((PathWithMask.indexOf('*') != -1) || (PathWithMask.indexOf('?') != -1))
        {
            args[0] = ExtractFilePathEx(PathWithMask, PathDelimiter);
            args[1] = ExtractFileNameEx(PathWithMask, PathDelimiter);
        }
        else
        {
            args[0] = PathWithMask;
            args[1] = "";
        }
    }

    //-----------------------------------------------------------------------------
    static private String ExcludeLeadingPathDelimiterEx(String Path, char PathDelimiter)
    {
        if (Path.equals("")) return "";
        return Path.indexOf(PathDelimiter) == 0 ? Path.substring(1, Path.length() - 1) : Path;
    }

    //-----------------------------------------------------------------------------
    static private String ExcludeTralingPathDelimiterEx(String Path, char PathDelimiter)
    {
        if (Path.equals("")) return "";
        return Path.lastIndexOf(PathDelimiter) == Path.length() - 1 ? Path.substring(0, Path.length() - 1) : Path;
    }

    //-----------------------------------------------------------------------------
    static private String IncludeTralingPathDelimiterEx(String Path, char PathDelimiter)
    {
        return Path.lastIndexOf(PathDelimiter) == Path.length() - 1 ? Path : Path + PathDelimiter;
    }

    //-----------------------------------------------------------------------------
    static private String IncludeLeadingPathDelimiterEx(String Path, char PathDelimiter)
    {
        return Path.indexOf(PathDelimiter) == 0 ? Path : PathDelimiter + Path;
    }

    static private String CombinePathEx(String Part1, String Part2, char PathDelimiter)
    {
        String Result;
        Result = ExcludeTralingPathDelimiterEx(Part1, PathDelimiter) + PathDelimiter + ExcludeLeadingPathDelimiterEx(Part2, PathDelimiter);
        return ExcludeTralingPathDelimiterEx(Result, PathDelimiter);
    }

    //-----------------------------------------------------------------------------
    static private int AskYNA(String Question) throws Exception
    {
        System.out.println(String.format("%s? (yna) ", Question));
        BufferedReader scanner = new BufferedReader(new InputStreamReader(System.in));
        String Answer = scanner.readLine();

        if (Answer.length() > 1)
            return ANSWER_UNKNOWN;
        else
            switch (Answer.charAt(0))
            {
                case 'y':
                case 'Y':
                    return ANSWER_YES;
                case 'n':
                case 'N':
                    return ANSWER_NO;
                case 'a':
                case 'A':
                    return ANSWER_ALL;
                default:
                    return ANSWER_UNKNOWN;
            }
    }

    //-----------------------------------------------------------------------------
    static private boolean FlagOn(int Flag, int Flags)
    {
        return ((Flags & Flag) == Flag);
    }

    //------------------------------------------------------------------------------
    private static boolean WidecadeMatch(String s, String p) {
        String tp = "";

        for (int i = 0; i < p.length(); i++) {
            if (p.charAt(i) == '*') {
                tp += '*';
                while (i < p.length() && p.charAt(i) == '*') i++;
            }
            if (i < p.length()) {
                tp += p.charAt(i);
            }
        }
        p = tp;

        boolean[][] f = new boolean[s.length() + 1][p.length() + 1];
        f[0][0] = true;

        if (p.length() > 0 && p.charAt(0) == '*') {
            f[0][1] = true;
        }

        for (int i = 1; i <= s.length(); i++) {
            for (int j = 1; j <= p.length(); j++) {
                if (p.charAt(j - 1) == '*') {
                    f[i][j] = f[i - 1][j - 1] || f[i - 1][j] || f[i][j - 1];
                } else {
                    f[i][j] = f[i - 1][j - 1] && (s.charAt(i - 1) == p.charAt(j - 1) || p.charAt(j - 1) == '?');
                }
            }
        }

        return f[s.length()][p.length()];
    }

    //-----------------------------------------------------------------------------
    static private int IterateVault(String FilePath, Method IterateFunc, boolean ProcessSubfolders, Cbvault Vault) throws Exception
    {
        long SearchData;
        String SearchTemplate;
        String RootDir, Mask;
        boolean MaskSpecified;
        boolean ProceedFilePath = false;
        int Result = 0;
        String[] SplitArgs = new String[2];
        SplitPathWithMaskEx(FilePath, SplitArgs, (char)Vault.getPathSeparator());
        RootDir = SplitArgs[0];
        Mask = SplitArgs[1];
        MaskSpecified = (Mask.length() > 0);
        if (!MaskSpecified)
        {
            if ((Vault.getFileAttributes(FilePath) & Constants.VAULT_FATTR_DIRECTORY) == Constants.VAULT_FATTR_DIRECTORY)
            {
                Mask = "*";
                ProceedFilePath = true;
            }
        }

        SearchTemplate = CombinePathEx(RootDir, Mask, (char)Vault.getPathSeparator());
        SearchData = Vault.findFirst(SearchTemplate, Constants.VAULT_FATTR_ANY_FILE, 0);
        if (SearchData != -1)
        {
            try
            {
                do
                {
                    if (((Vault.getSearchResultAttributes(SearchData) & Constants.VAULT_FATTR_DIRECTORY) == Constants.VAULT_FATTR_DIRECTORY) && ProcessSubfolders)
                    {
                        Result = IterateVault(CombinePathEx(Vault.getSearchResultFullName(SearchData), Mask, (char)Vault.getPathSeparator()), IterateFunc, ProcessSubfolders, Vault);
                        if ((Result != 0) && (Result != Constants.VAULT_ERR_FILE_NOT_FOUND))
                            return Result;
                    }
                    Result = (Integer) IterateFunc.invoke(null, SearchData, Vault);
                    if (Result != 0)
                        return Result;
                }
                while (Vault.findNext(SearchData));
            }
            finally
            {
                Vault.findClose(SearchData);
            }
        }

        if (ProceedFilePath)
        {
            SearchData = Vault.findFirst(FilePath, Constants.VAULT_FATTR_ANY_FILE, 0);
            if (SearchData != -1)
            {
                try
                {
                    Result = (Integer)IterateFunc.invoke(null, SearchData, Vault);
                }
                finally
                {
                    Vault.findClose(SearchData);
                }
            }
        }

        return Result;
    }

    //-----------------------------------------------------------------------------
    static private int IterateFiles(String FilePath, Method IterateFunc, boolean Recursive, Cbvault Vault) throws  Exception
    {
        int Result = 0;
        String RootDir = "", Mask = "";
        boolean MaskSpecified;
        boolean ProceedFilePath = false;
        boolean IsDirectory;
        String[] SplitArgs = new String[2];
        SplitPathWithMaskEx(FilePath.replace('/', '\\'), SplitArgs, '\\');
        RootDir = SplitArgs[0];
        Mask = SplitArgs[1];
        MaskSpecified = Mask.length() > 0;

        if (isNullOrEmpty(RootDir))
            RootDir = ".";

        File file = new File(RootDir);
        if (!MaskSpecified)
        {
            IsDirectory = file.exists() && file.isDirectory();
            if (IsDirectory)
            {
                Mask = "*";
                MaskSpecified = true;
            }
            ProceedFilePath = !IsDirectory;
        }
        else
            IsDirectory = file.exists() && file.isDirectory();

        if (Recursive)
        {
            File[] subFiles = file.listFiles();
            if (subFiles != null)
            {
                for (File f : subFiles )
                {
                    if (f.isDirectory())
                    {
                        Result = AddDirectory(f.getPath(), Vault);
                        // or Path.Combine(Dir, Mask)
                        Result = IterateFiles(CombinePathEx(f.getPath(), Mask, '\\'), IterateFunc, Recursive, Vault);
                        if (Result != 0) return Result;

                        Result = (Integer)IterateFunc.invoke(null, f.getPath(), true, Vault);
                        if (Result != 0) return Result;
                    }
                }
            }
        }

        File fileRoot = new File(RootDir);
        File[] subFiles = fileRoot.listFiles();
        if (subFiles != null)
        {
            for (File f : subFiles)
            {
                if (!f.isDirectory())
                {
                    String name = f.getName();
                    if (WidecadeMatch(name, Mask))
                    {
                        Result = (Integer)IterateFunc.invoke(null, f.getPath(), false, Vault);
                        if (Result != 0) return Result;
                    }
                }
            }
        }

        if (ProceedFilePath)
            Result = (Integer)IterateFunc.invoke(null, FilePath, false, Vault);

        return Result;
    }

    //-----------------------------------------------------------------------------
    static private int OpenVault(String VaultName, Cbvault[] VaultArg, boolean ExistingOnly)
    {
        int Result = 0;
        VaultArg[0] = new Cbvault();
        Cbvault Vault = VaultArg[0];
        try
        {
            Vault.setVaultFile(VaultName);
            File file = new File(Vault.getVaultFile());

            if (ExistingOnly)
            {
                Vault.openVault(Constants.VAULT_OM_OPEN_EXISTING, Constants.VAULT_JM_NONE);
            }
            else
            if (OptionOverwriteVault || !file.exists())
            {
                if (OptionPageSize != 0)
                {
                    Vault.setPageSize(OptionPageSize);
                }
                if (OptionFixedVaultSize != 0)
                {
                    Vault.setVaultSizeMax(OptionFixedVaultSize);
                    Vault.setVaultSizeMin(OptionFixedVaultSize);
                }
                Vault.openVault(Constants.VAULT_OM_CREATE_ALWAYS, Constants.VAULT_JM_NONE);
            }
            else
            {
                Vault.openVault(Constants.VAULT_OM_OPEN_ALWAYS, Constants.VAULT_JM_NONE);
            }
        }
        catch (CBFSVaultException e)
        {
            System.out.println(String.format("Error opening vault (%d: %s)", e.getCode(), e.getMessage()));
            Result = e.getCode();
        }
        catch (Exception e)
        {
            System.out.println(String.format("Error opening vault (%s)", e.getMessage()));
            Result = -1;
        }

        return Result;
    }

    //-----------------------------------------------------------------------------
    static private int AddDirectory(String DirectoryName, Cbvault Vault)
    {
        int Result = 0;
        boolean DirectoryExists;
        String ErrorMessage = null;
        String VaultDirectoryName = OSPathToVaultPath(DirectoryName);
        try {
            DirectoryExists = Vault.fileExists(VaultDirectoryName);
        } catch (CBFSVaultException e) {
            DirectoryExists = false;
        }
        if (!DirectoryExists) {
            System.out.print(String.format("Adding dir  %s", VaultDirectoryName));
            try {
                Vault.createDirectory(VaultDirectoryName, true);
            } catch (CBFSVaultException e) {
                Result = e.getCode();
                ErrorMessage = e.getMessage();
            } catch (Exception e) {
                Result = -1;
                ErrorMessage = e.getMessage();
            }
            if (Result == 0)
                System.out.println(" - ok");
            else
                System.out.println(String.format(" - error: %s", ErrorMessage));
        }
        return Result;
    }

    //-----------------------------------------------------------------------------
    static private int AddFile(String FileName, Cbvault Vault) throws Exception
    {
        int Result = 0;
        String ErrorMessage = null;
        String VaultFileName;

        VaultFileName = OSPathToVaultPath(FileName);

        Result = AddDirectory(ExtractFilePathEx(VaultFileName, '\\'), Vault);
        if (Result == 0) {
            boolean AddFile, FileExists;
            try {
                FileExists = Vault.fileExists(VaultFileName);
            } catch (CBFSVaultException e) {
                FileExists = false;
            }
            if (FileExists) {
                if (OptionAlwaysYes)
                    AddFile = true;
                else {
                    if (_OptionOverwriteFilesSet) {
                        if (OptionOverwriteFiles)
                            AddFile = true;
                        else {
                            System.out.println(String.format("File %s already exists. Skipped", VaultFileName));
                            AddFile = false;
                        }

                    } else {
                        switch (AskYNA(String.format("File %s already exists. Overwrite", FileName))) {
                            case ANSWER_YES:
                                AddFile = true;
                                break;
                            case ANSWER_ALL:
                                AddFile = true;
                                _OptionOverwriteFilesSet = true;
                                OptionOverwriteFiles = true;
                                break;
                            default:  //ANSWER_UNKNOWN, ANSWER_NO
                                AddFile = false;
                                System.out.println("Skipped");
                                break;
                        }
                    }
                }
                if (!AddFile) return 0;
                Vault.deleteFile(VaultFileName);
            }

            System.out.print(String.format("Adding file %s", VaultFileName));
            try {
                File TheFile = new File(FileName);
                FileInputStream TheFileStream = new FileInputStream(TheFile);
                try {
                    Vault.createDirectory(ExtractFilePathEx(VaultFileName, (char) Vault.getPathSeparator()), true);
                    CBFSVaultStream TheVaultStream = Vault.openFileEx(VaultFileName, Constants.VAULT_FOM_CREATE_NEW, false, true, true, true,
                            OptionPassword == "" ? Constants.VAULT_EM_NONE : Constants.VAULT_EM_DEFAULT,
                            OptionPassword,
                            OptionCompressionLevel == -1 ? Constants.VAULT_CM_NONE : Constants.VAULT_CM_ZLIB,
                            OptionCompressionLevel, 1);
                    try {
                        byte[] Buffer = new byte[BUFFER_SIZE];
                        long TotalRead = 0;
                        int BytesRead;
                        while (TotalRead < TheFile.length()) {
                            BytesRead = TheFileStream.read(Buffer);
                            if (BytesRead != 0) {
                                byte[] WBuffer = Buffer;
                                if (BytesRead != Buffer.length) {
                                    WBuffer = new byte[BytesRead];
                                    System.arraycopy(Buffer, 0, WBuffer, 0, BytesRead);
                                }
                                BytesRead = TheVaultStream.write(WBuffer);
                                TotalRead += BytesRead;
                            }
                        }
                    } finally {
                        TheVaultStream.close();
                    }
                } finally {
                    TheFileStream.close();
                }
            } catch (CBFSVaultException e) {
                Result = e.getCode();
                ErrorMessage = e.getMessage();
            } catch (Exception e) {
                Result = -1;
                ErrorMessage = e.getMessage();
            }
            if (Result == 0)
                System.out.println(" - ok");
            else
                System.out.println(String.format(" - error: %s", ErrorMessage));
        }

        return Result;

    }

    //-----------------------------------------------------------------------------
    static public int AddFileObject(String FilePath, boolean IsDirectory, Cbvault Vault) throws Exception
    {
        int Result = 0;
        String ErrorMessage = null;

        File file = new File(FilePath);
        if (IsDirectory)
        {
            Result = AddDirectory(FilePath, Vault);
            if ((Result == 0) && OptionDeleteAfterArchiving)
            {
                file.delete();
            }
        }
        else
        {
            Result = AddFile(FilePath, Vault);
            if ((Result == 0) && OptionDeleteAfterArchiving)
            {
                file.delete();
            }
        }
        return Result;
    }

    //-----------------------------------------------------------------------------
    static public int DeleteFileObject(long SearchData, Cbvault Vault)
    {
        int Result = 0;
        String ErrorMessage = null;

        try
        {
            String fullName = Vault.getSearchResultFullName(SearchData);

            if ((Vault.getSearchResultAttributes(SearchData) & Constants.VAULT_FATTR_DIRECTORY) == Constants.VAULT_FATTR_DIRECTORY)
                System.out.println(String.format("Deleting directory %s", fullName));
            else
                System.out.println(String.format("Deleting file %s", fullName));

            Vault.deleteFile(fullName);
        }
        catch (CBFSVaultException e)
        {
            Result = e.getCode();
            ErrorMessage = e.getMessage();
        }
        catch (Exception e)
        {
            Result = -1;
            ErrorMessage = e.getMessage();
        }
        if (Result == 0)
            System.out.println(" - ok");
        else
            System.out.println(String.format(" - error: %s", ErrorMessage));
        return Result;
    }

    //-----------------------------------------------------------------------------
    static int ExtractFile(long SearchData, Cbvault Vault) throws Exception
    {
        int Result = 0;
        String OSFileName, OSPath;
        String ErrorMessage = null;

        if (TheCommand == Commands.Extract)
        {
            OSFileName = Vault.getSearchResultName(SearchData);
        }
        else if (TheCommand == Commands.eXtractWithPath)
        {
            OSFileName = Vault.getSearchResultFullName(SearchData).replace((char) Vault.getPathSeparator(), '\\');
        }
        else
        {
            throw new Exception(String.format("ExtractFileFromVault error. Invalid command: %d", TheCommand));
        }

        File outDir = new File(_OutputPath);
        OSFileName = CombinePathEx(outDir.getAbsolutePath(), OSFileName, '\\');
        File osFile = new File(OSFileName);
        if (osFile.exists())
        {
            boolean ExtractThisFile;
            if (OptionAlwaysYes)
            {
                ExtractThisFile = true;
            }
            else if (_OptionOverwriteFilesSet)
            {
                if (OptionOverwriteFiles)
                {
                    ExtractThisFile = true;
                }
                else
                {
                    ExtractThisFile = false;
                    System.out.println("File {0} already exists. Skipped");
                }
            }
            else
            {
                switch (AskYNA(String.format("File %s already exists. Overwrite", OSFileName)))
                {
                    case ANSWER_YES:
                        ExtractThisFile = true;
                        break;
                    case ANSWER_ALL:
                        ExtractThisFile = true;
                        _OptionOverwriteFilesSet = true;
                        OptionOverwriteFiles = true;
                        break;
                    default:  //ANSWER_UNKNOWN, ANSWER_NO
                        ExtractThisFile = false;
                        System.out.println("Skipped");
                        break;
                }

            }
            if (!ExtractThisFile)
                return 0;
        }

        OSPath = ExtractFilePathEx(OSFileName, '\\');
        if (!OSPath.equals(""))
        {
            File dir = new File(OSPath);
            dir.mkdir();
        }

        String fullName = Vault.getSearchResultFullName(SearchData);

        System.out.println(String.format("Extracting file %s", fullName));
        try
        {
            CBFSVaultStream TheVaultStream;
            TheVaultStream = Vault.openFile(fullName, Constants.VAULT_FOM_OPEN_EXISTING, true, false, OptionPassword);
            try
            {
                File TheFile = new File(OSFileName);
                if (!TheFile.exists()) TheFile.createNewFile();
                FileOutputStream TheFileStream = new FileOutputStream(TheFile);
                try
                {
                    long TotalRead = 0;
                    int BytesRead;
                    while (TotalRead < TheVaultStream.getLength())
                    {
                        byte[] Buffer = TheVaultStream.read(BUFFER_SIZE);
                        BytesRead = Buffer.length;
                        TheFileStream.write(Buffer, 0, BytesRead);
                        TotalRead += BytesRead;
                    }
                }
                finally
                {
                    TheFileStream.close();
                }
            }
            finally
            {
                TheVaultStream.close();
            }
        }
        catch (CBFSVaultException e)
        {
            Result = e.getCode();
            ErrorMessage = e.getMessage();
        }
        catch (Exception e)
        {
            Result = -1;
            ErrorMessage = e.getMessage();
        }
        if (Result == 0)
            System.out.println(" - ok");
        else
            System.out.println(String.format(" - error: %s", ErrorMessage));

        return Result;

    }

    //-----------------------------------------------------------------------------
    static int ExtractDirectory(long SearchData, Cbvault Vault)
    {
        int Result = 0;
        String OSDirectoryName;
        String ErrorMessage = null;

        if (TheCommand == Commands.Extract)
            return 0;

        try
        {
            String fullName = Vault.getSearchResultFullName(SearchData);

            OSDirectoryName = fullName.replace((char) Vault.getPathSeparator(), '\\');
            OSDirectoryName = CombinePathEx(_OutputPath, OSDirectoryName, '\\');
            System.out.println(String.format("Extracting dir  %s", fullName));

            File dir = new File(OSDirectoryName);
            dir.mkdir();
        }
        catch (CBFSVaultException e)
        {
            Result = e.getCode();
            ErrorMessage = e.getMessage();
        }
        catch (Exception e)
        {
            Result = -1;
            ErrorMessage = e.getMessage();
        }
        if (Result == 0)
            System.out.println(" - ok");
        else
            System.out.println(String.format(" - error: %s", ErrorMessage));
        return Result;
    }

    //-----------------------------------------------------------------------------
    static public int ExtractFileObject(long SearchData, Cbvault Vault) throws Exception
    {
        if ((Vault.getSearchResultAttributes(SearchData) & Constants.VAULT_FATTR_DIRECTORY) == Constants.VAULT_FATTR_DIRECTORY)
            return ExtractDirectory(SearchData, Vault);
        else
            return ExtractFile(SearchData, Vault);
    }

    //-----------------------------------------------------------------------------
    static public int ListFile(long SearchData, Cbvault Vault) throws Exception
    {
        if ((Vault.getSearchResultAttributes(SearchData) & Constants.VAULT_FATTR_DIRECTORY) == Constants.VAULT_FATTR_DIRECTORY)
            System.out.println(Vault.getSearchResultFullName(SearchData));
        else
            System.out.println(String.format("%-68s %12d", Vault.getSearchResultFullName(SearchData), Vault.getSearchResultSize(SearchData)));
        return 0;
    }

    //-----------------------------------------------------------------------------
    static public int TestFile(long SearchData, Cbvault Vault) throws Exception
    {
        int Result = 0;
        String ErrorMessage = null;

        if ((Vault.getSearchResultAttributes(SearchData) & Constants.VAULT_FATTR_DIRECTORY) == Constants.VAULT_FATTR_DIRECTORY)
            return 0;

        String fullName = Vault.getSearchResultFullName(SearchData);
        System.out.println(String.format("Testing file %s", fullName));
        try
        {
            CBFSVaultStream VaultStream = Vault.openFile(fullName, Constants.VAULT_FOM_OPEN_EXISTING, true, false, OptionPassword);
            try
            {
                long TotalRead = 0;
                int BytesRead;
                while (TotalRead < VaultStream.getLength())
                {
                    byte[] Buffer = VaultStream.read(BUFFER_SIZE);
                    BytesRead = Buffer.length;
                    TotalRead += BytesRead;
                }
            }
            finally
            {
                VaultStream.close();
            }
        }
        catch (CBFSVaultException e)
        {
            Result = e.getCode();
            ErrorMessage = e.getMessage();
        }
        catch (Exception e)
        {
            Result = -1;
            ErrorMessage = e.getMessage();
        }
        if (Result == 0)
            System.out.println(" - ok");
        else
            System.out.println(String.format(" - error: %s", ErrorMessage));

        return Result;
    }

    //-----------------------------------------------------------------------------
    static public int AddFiles(String VaultName, List<String> FileNames)
    {
        int Result;
        Cbvault Vault = null;
        Cbvault[] VaultArg = new Cbvault[1];

        Result = OpenVault(VaultName, VaultArg, false);

        if (Result != 0)
            return Result;

        Vault = VaultArg[0];

        try
        {
            Method method = vaultmgr.class.getMethod("AddFileObject", String.class, boolean.class, Cbvault.class);
            for (String FileName : FileNames)
            {
                IterateFiles(FileName, method, OptionRecursive, Vault);
            }

            Vault.closeVault();
        }
        catch (CBFSVaultException e)
        {
            System.out.println(String.format("Error AddFiles (%d: %s)", e.getCode(), e.getMessage()));
            Result = e.getCode();
        }
        catch (InvocationTargetException e)
        {
            Throwable target = e.getTargetException();
            if (target instanceof  CBFSVaultException)
            {
                CBFSVaultException ex = (CBFSVaultException)target;
                System.out.println(String.format("Error AddFiles (%d: %s)", ex.getCode(), ex.getMessage()));
                Result = ex.getCode();
            }
            else
            {
                System.out.println(String.format("Error AddFiles (%s)", e.getMessage()));
                Result = -1;
            }
        }
        catch (Exception e)
        {
            System.out.println(String.format("Error AddFiles (%s)", e.getMessage()));
            Result = -1;
        }

        return Result;
    }

    //-----------------------------------------------------------------------------
    static public int DeleteFiles(String VaultName, List<String> FileNames)
    {
        int Result;
        Cbvault Vault = null;
        Cbvault[] VaultArg = new Cbvault[1];

        Result = OpenVault(VaultName, VaultArg, true);

        if (Result != 0)
            return Result;

        Vault = VaultArg[0];

        try
        {
            Method method = vaultmgr.class.getMethod("DeleteFileObject", long.class, Cbvault.class);
            for (String FileName : FileNames)
            {
                Result = IterateVault(FileName, method, true, Vault);
                if (Result != 0)
                    return Result;
            }
            Vault.closeVault();
        }
        catch (CBFSVaultException e)
        {
            System.out.println(String.format("Error DeleteFiles (%d: %s)", e.getCode(), e.getMessage()));
            Result = e.getCode();
        }
        catch (Exception e)
        {
            System.out.println(String.format("Error DeleteFiles (%s)", e.getMessage()));
            Result = -1;
        }
        return Result;
    }

    //-----------------------------------------------------------------------------
    static public int ExtractFiles(String VaultName, List<String> FileNames)
    {
        int Result;
        Cbvault Vault = null;
        Cbvault[] VaultArg = new Cbvault[1];

        Result = OpenVault(VaultName, VaultArg, true);

        if (Result != 0)
            return Result;

        Vault = VaultArg[0];

        if (FileNames.size() == 0)
            FileNames.add("*");

        try
        {
            Method method = vaultmgr.class.getMethod("ExtractFileObject", long.class, Cbvault.class);
            for (String FileName : FileNames)
            {
                Result = IterateVault(FileName, method, true, Vault);
                if (Result != 0)
                    return Result;
            }
            Vault.closeVault();
        }
        catch (CBFSVaultException e)
        {
            System.out.println(String.format("Error ListFiles (%d: %s)", e.getCode(), e.getMessage()));
            Result = e.getCode();
        }
        catch (Exception e)
        {
            System.out.println(String.format("Error ListFiles (%s)", e.getMessage()));
            Result = -1;
        }

        return Result;
    }

    //-----------------------------------------------------------------------------
    static public int ListFiles(String VaultName, List<String> FileNames)
    {
        int Result;
        Cbvault Vault = null;
        Cbvault[] VaultArg = new Cbvault[1];

        Result = OpenVault(VaultName, VaultArg, true);
        if (Result != 0)
            return Result;

        Vault = VaultArg[0];

        if (FileNames.size() == 0)
            FileNames.add("*");

        System.out.println(String.format("%-68s %12s", "File Name", "Size"));
        System.out.println("--------------------------------------------------------------------------------");

        try
        {
            Method method = vaultmgr.class.getMethod("ListFile", long.class, Cbvault.class);
            for (String FileName : FileNames)
            {
                Result = IterateVault(FileName, method, true, Vault);
                if (Result != 0)
                    return Result;
            }
            Vault.closeVault();
        }
        catch (CBFSVaultException e)
        {
            System.out.println(String.format("Error ListFiles (%d: %s)", e.getCode(), e.getMessage()));
            Result = e.getCode();
        }
        catch (Exception e)
        {
            System.out.println(String.format("Error ListFiles (%s)", e.getMessage()));
            Result = -1;
        }

        return Result;
    }

    //-----------------------------------------------------------------------------
    static public int TestFiles(String VaultName, List<String> FileNames)
    {
        int Result;
        Cbvault Vault = null;
        Cbvault[] VaultArg = new Cbvault[1];

        Result = OpenVault(VaultName, VaultArg, true);

        if (Result != 0)
            return Result;

        Vault = VaultArg[0];

        if (FileNames.size() == 0)
            FileNames.add("*");

        System.out.println(String.format("%-68s %12s", "File Name", "Size"));
        System.out.println("--------------------------------------------------------------------------------");

        try
        {
            Method method = vaultmgr.class.getMethod("TestFile", long.class, Cbvault.class);
            for (String FileName : FileNames)
            {
                IterateVault(FileName, method, true, Vault);
            }
            Vault.closeVault();
        }
        catch (CBFSVaultException e)
        {
            System.out.println(String.format("Error TestFiles (%d: %s)", e.getCode(), e.getMessage()));
            Result = e.getCode();
        }
        catch (Exception e)
        {
            System.out.println(String.format("Error TestFiles (%s)", e.getMessage()));
            Result = -1;
        }

        return Result;
    }

    //-----------------------------------------------------------------------------
    static private boolean SetOption(char Option, String Parameter)
    {
        switch (Option)
        {
            case 'c':
                try
                {
                    OptionCompressionLevel = Integer.parseInt(Parameter);
                }
                catch (Exception e)
                {
                    return false;
                }

                if (!((OptionCompressionLevel == 0) ||
                        (OptionCompressionLevel == 1) ||
                        (OptionCompressionLevel == 3) ||
                        (OptionCompressionLevel == 4)))
                {
                    return false;
                }
                break;
            case 'd':
                OptionDeleteAfterArchiving = true;
                break;
            case 'f':
                try
                {
                    OptionFixedVaultSize = Integer.parseInt(Parameter) * 1024*1024;
                }
                catch (Exception e)
                {
                    return false;
                }

                if (OptionFixedVaultSize <= 0)
                {
                    return false;
                }
                break;
            case 'g':
                try
                {
                    OptionPageSize = Integer.parseInt(Parameter);
                }
                catch (Exception e)
                {
                    return false;
                }

                if (OptionPageSize <512 || OptionPageSize > 65536)
                {
                    return false;
                }
                break;
            case 'l':
                OptionLowerCase = true;
                break;
            case 'o':
                OptionOutputPath = ConvertRelativePathToAbsolute(Parameter);
                break;
            case 'p':
                OptionPassword = Parameter;
                break;
            case 'r':
                OptionRecursive = true;
                break;
            case 's':
                if (Parameter.length() != 1)
                    return false;
                switch (Parameter.charAt(0))
                {
                    case '+':
                        OptionOverwriteVault = true;
                        break;
                    case '-':
                        OptionOverwriteVault = false;
                        break;
                    default:
                        return false;
                }
                break;
            case 'u':
                OptionUpperCase = true;
                break;
            case 'w':
                if (Parameter.length() != 1)
                    return false;
                switch (Parameter.charAt(0))
                {
                    case '+':
                        OptionOverwriteFiles = true;
                        _OptionOverwriteFilesSet = true;
                        break;
                    case '-':
                        OptionOverwriteFiles = false;
                        _OptionOverwriteFilesSet = true;
                        break;
                    default:
                        return false;
                }
                break;
            case 'y':
                OptionAlwaysYes = true;
                break;
            default:
                return false;
        }
        return true;
    }

    private static boolean isDriveLetter(String path) {
        if (path == null || path.isEmpty() || path.length() != 2)
            return false;

        char c = path.charAt(0);
        if ((((int)c >= (int) 'A' && (int)c <= (int) 'Z') ||
                ((int)c >= (int) 'a' && (int)c <= (int) 'z')) &&
                (path.charAt(1) == ':'))
            return true;
        else
            return false;
    }

    private static String ConvertRelativePathToAbsolute(String path) {
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
                    System.out.println("The network folder '" + res + "' does not exist.");
                } else if (!file.exists()) {
                    System.out.println("The path '" + res + "' does not exist.");
                }
            }
        }
        return res;
    }

    //-----------------------------------------------------------------------------

    public static void main(String[] args) {
        int Result = 0;
        char InputCommand;
        char Option;
        String VaultName;
        String Parameter;
        int ParNo;
        boolean SkipOptions;
        List<String> FileNames = new ArrayList<String>();
        Method CmdFunc;

        if (args.length < 2)
        {
            Usage();
            return;
        }

        if (args[0].length() != 1)
        {
            System.out.println(String.format("Invalid command \"%s\"", args[0]));
            return;
        }

        InputCommand = Character.toLowerCase(args[0].charAt(0));
        if (AllCommands.indexOf(InputCommand) == -1)
        {
            System.out.println(String.format("Invalid command \"%s\"", InputCommand));
            return;
        }

        ParNo = 1;
        SkipOptions = false;
        while(ParNo < args.length)
        {
            Parameter = args[ParNo];
            if ((Parameter.length() < 2) || (Parameter.charAt(0) != '-'))
                break;

            Option = Character.toLowerCase(Parameter.charAt(1));
            if (Option == '-')
                SkipOptions = true;
            if (SkipOptions)
            {
                ParNo++;
                continue;
            }

            if (AllOptions.indexOf(Option) == -1)
            {
                System.out.println(String.format("Invalid option \"%s\"", Option));
                return;
            }

            if (AllOptionsWithParameters.indexOf(Option) != -1)
            {
                if ((Parameter.length() < 3) || (!SetOption(Option, Parameter.substring(2))))
                    System.out.println(String.format("Invalid option \"%s\"", Option));
            }
            else
            {
                if (!SetOption(Option, ""))
                    System.out.println(String.format("Invalid option \"%s\"", Option));
            }

            ParNo++;
        }

        if (args.length <= ParNo)
        {
            Usage();
            return;
        }

        VaultName = ConvertRelativePathToAbsolute(args[ParNo++]);

        for (int i = ParNo; i < args.length; i++)
        {
            FileNames.add(args[i]);
        }

        try
        {
            switch (InputCommand)
            {
                case 'a':
                    TheCommand = Commands.Add;
                    CmdFunc = vaultmgr.class.getMethod("AddFiles", String.class, List.class);
                    break;
                case 'd':
                    TheCommand = Commands.Delete;
                    CmdFunc = vaultmgr.class.getMethod("DeleteFiles", String.class, List.class);
                    break;
                case 'e':
                    TheCommand = Commands.Extract;
                    CmdFunc = vaultmgr.class.getMethod("ExtractFiles", String.class, List.class);
                    break;
                case 'l':
                    TheCommand = Commands.List;
                    CmdFunc = vaultmgr.class.getMethod("ListFiles", String.class, List.class);
                    break;
                case 't':
                    TheCommand = Commands.Test;
                    CmdFunc = vaultmgr.class.getMethod("TestFiles", String.class, List.class);
                    break;
                case 'x':
                    TheCommand = Commands.eXtractWithPath;
                    CmdFunc = vaultmgr.class.getMethod("ExtractFiles", String.class, List.class);
                    break;
                default:
                    System.out.println(String.format("Invalid command \"%s\"", InputCommand));
                    return;
            }

            if ((TheCommand == Commands.Extract) || (TheCommand == Commands.eXtractWithPath))
            {
                File directory = new File(OptionOutputPath);
                _OutputPath = directory.getAbsolutePath(); // convert relative path to absolute
            }

            Result = (Integer) CmdFunc.invoke(null, VaultName, FileNames);
        }
        catch (Exception e)
        {
            Result = -1;
            System.out.println(String.format("Error (%s)", e.getMessage()));
        }
        if (Result != 0)
            System.out.println(String.format("Error Code: %d", Result));
    }
}





