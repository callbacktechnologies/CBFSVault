/*
 * CBFS Vault 2022 C++ Edition - Sample Project
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

#include "stdafx.h"

#ifdef _UNICODE
#include "../../include/unicode/cbfsvault.h"
typedef __int64 int64;
#else
#include "../../include/cbfsvault.h"
#endif

#include <string>
#include <algorithm>
#include <sstream>
#include <iostream>
#include <fstream>
#include <vector>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifndef _WIN32
#include <unistd.h>
#include <dirent.h>
#else
#include <io.h>
#include <direct.h>
#include "dirent.h"
//#include <windows.h> 
#endif

using namespace std;

#ifdef _UNICODE
typedef std::wstring comp_string;
#else
typedef std::string comp_string;
#endif

#if defined (_MSC_VER)
#pragma warning(disable: 4996)
#endif

enum Commands
{
    Add,
    Delete,
    Extract,
    List,
    Test,
    eXtract
};

typedef int(*IterateFilesFunction)(string RootDir, string FilePath, bool IsDirectory, ::CBVault &Vault);
typedef int(*IterateVaultFunction)(int64 SearchDataHandle, ::CBVault &Vault);
typedef int(*CommandFunction)(string VaultName, vector<string> FileNames);

const string AllCommands("adeltx");
const string AllOptions("cdfgloprqsyuw-");
const string AllOptionsWithParameters("cfgopsw");

const int BUFFER_SIZE = 4 * 1024 * 1024;

const int ANSWER_UNKNOWN = 0;
const int ANSWER_YES = 1;
const int ANSWER_NO = 2;
const int ANSWER_ALL = 3;

Commands g_Command;

bool   g_OptionAlwaysYes = false;
int    g_OptionCompressionLevel = -1;
bool   g_OptionDeleteAfterArchiving = false;
int    g_OptionFixedVaultSize = 0;
bool   g_OptionLowerCase = false;
string g_OptionOutputPath = "";
bool   g_OptionOverwriteFiles = false;
bool   g_OptionOverwriteVault = false;
int    g_OptionPageSize = 4096;
string g_OptionPassword = "";
bool   g_OptionRecursive = false;
bool   g_OptionUpperCase = false;

bool   g__OptionOverwriteFilesSet = false;
bool   g__ExtractWithFullPath = false;
string g__OutputPath = "";


//-----------------------------------------------------------------------------
void Usage()
{
    cout << "Usage: vaultmgr <command> [-<switch 1> ... -<switch N>] <vault> [<files>...]\n" << endl;
    cout << "<Commands>" << endl;
    cout << "  a             Add files to vault" << endl;
    cout << "  d             Delete files from vault" << endl;
    cout << "  e             Extract files from vault" << endl;
    cout << "  l             List contents of vault" << endl;
    cout << "  t             Test integrity of vault" << endl;
    cout << "  x             eXtract files with full pathname" << endl;

    cout << "<Switches>" << endl;
    cout << "  -             Stop switches scanning" << endl;

    cout << "  l             Convert names to lower case" << endl;
    cout << "  u             Convert names to upper case" << endl;
    cout << "  d             Delete files after archiving" << endl;
    cout << "  c<0-9>        set Compression level (0 - default)" << endl;
    cout << "  f<size>       set the Fixed size of the vault in MB" << endl;
    cout << "  g<page size>  set size of the paGe for new vault (4096 - default)" << endl;
    cout << "  p<password>   set Password" << endl;
    cout << "  o<path>       set Output directory" << endl;
    cout << "  r             Recurse subdirectories" << endl;
    cout << "  s+            overwrite existing Vault" << endl;
    cout << "  s-            do not overwrite Vault (default)" << endl;
    cout << "  y             Assume Yes on all queries" << endl;
    cout << "  w+            overWrite existing files" << endl;
    cout << "  w-            do not overWrite existing files (default)" << endl;
}

//-----------------------------------------------------------------------------
bool SetPositiveOption(char Option, string Parameter, int &OptionToSet)
{
    istringstream iss(Parameter);
    if ((iss >> std::dec >> OptionToSet).fail())
        return false;
    if (OptionToSet <= 0)
        return false;
    return true;
}

//-----------------------------------------------------------------------------
bool SetSwitchOption(char Option, string Parameter, bool &OptionToSet)
{
    if (Parameter.length() != 1)
        return false;

    switch (Parameter[0])
    {
    case '+':
        OptionToSet = true;
        break;
    case '-':
        OptionToSet = false;
        break;
    default:
        return false;
    }
    return true;
}

//-----------------------------------------------------------------------------
bool SetOption(char Option, string Parameter)
{
    switch (Option)
    {
    case 'c':
        if (!SetPositiveOption(Option, Parameter, g_OptionCompressionLevel))
            return false;
        break;
    case 'd':
        g_OptionDeleteAfterArchiving = true;
        break;
    case 'f':
        if (!SetPositiveOption(Option, Parameter, g_OptionFixedVaultSize))
            return false;
        g_OptionFixedVaultSize = g_OptionFixedVaultSize * 1024 * 1024;
        break;
    case 'g':
        if (!SetPositiveOption(Option, Parameter, g_OptionPageSize))
            return false;
        if (g_OptionPageSize < 512 || g_OptionPageSize > 65536)
            return false; 
        break;
    case 'l':
        g_OptionLowerCase = true;
        break;
    case 'o':
        g_OptionOutputPath = Parameter;
        break;
    case 'p':
        g_OptionPassword = Parameter;
        break;
    case 'r':
        g_OptionRecursive = true;
        break;
    case 's':
        if (!SetSwitchOption(Option, Parameter, g_OptionOverwriteVault))
            return false;
        break;
    case 'u':
        g_OptionUpperCase = true;
        break;
    case 'w':
        if (!SetSwitchOption(Option, Parameter, g_OptionOverwriteFiles))
            return false;
        break;
    case 'y':
        g_OptionAlwaysYes = true;
        break;
    default:
        return false;
    }
    return true;
}

//-----------------------------------------------------------------------------
int AskYNA(string Question)
{
    string Answer;
    cout << Question << " (yna)? ";
    cin >> Answer;
    if (Answer.length() != 1)
        return ANSWER_UNKNOWN;

    switch (Answer[0])
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
bool IsMatchMask(const char *Name, const char *Mask)
{
    const char *MaskPtr, *NamePtr, *LastStarPtr;
    int LastStarDec;
    bool result;

    result = true;
    MaskPtr = Mask;
    NamePtr = Name;
    LastStarPtr = NULL;
    LastStarDec = 0;
    while (result && (*NamePtr != 0))
    {
        if (*MaskPtr == '?')
        {
            MaskPtr++;
            NamePtr++;
        }
        else if (*MaskPtr == '*')
        {
            LastStarPtr = MaskPtr;
            LastStarDec = 0;
            MaskPtr++;
            while (true)
            {
                if (*MaskPtr == '?')
                {
                    MaskPtr++;
                    NamePtr++;
                    LastStarDec++;
                    if (*NamePtr == 0)
                        break;
                }
                else if (*MaskPtr != '*')
                    break;
                else
                    MaskPtr++;
            }
            while ((*NamePtr != 0) && ((*MaskPtr == 0) || (*MaskPtr != *NamePtr)))
                NamePtr++;
        }
        else
        {
            result = (*MaskPtr == *NamePtr);
            if (result)
            {
                MaskPtr++;
                NamePtr++;
            }
            else
            {
                result = LastStarPtr != NULL;
                if (result)
                {
                    MaskPtr = LastStarPtr;
                    NamePtr -= LastStarDec;
                }
            }
        }
    }
    if (result)
    {
        while (*MaskPtr == '*')
            MaskPtr++;
        result = (*MaskPtr == 0);
    }
    return result;
}

#ifdef _WIN32
const char OSSeparator = '\\';
#else
const char OSSeparator = '/';
#endif

//-----------------------------------------------------------------------------
string ExcludeLeadingPathDelimiterEx(string Path)
{
    if (Path == "")
        return "";
    if ((Path.find('/') == 0) || (Path.find('\\') == 0))
        return Path.substr(1, Path.length() - 1);
    else
        return Path;
}

//-----------------------------------------------------------------------------
string IncludeLeadingPathDelimiterEx(string Path)
{
    if (Path == "")
        return "";
    if ((Path.find('/') == 0) || (Path.find('\\') == 0))
        return Path;
    else
        return OSSeparator + Path;
}

//-----------------------------------------------------------------------------
string ExcludeTralingPathDelimiterEx(string Path)
{
    if (Path == "")
        return "";
    if ((Path.find_last_of('/') == Path.length() - 1) ||
        (Path.find_last_of('\\') == Path.length() - 1))
        return Path.substr(0, Path.length() - 1);
    else
        return Path;
}

//-----------------------------------------------------------------------------
string IncludeTralingPathDelimiterEx(string Path)
{
    if (Path == "")
        return "";
    if ((Path.find_last_of('/') == Path.length() - 1) ||
        (Path.find_last_of('\\') == Path.length() - 1))
        return Path;
    else
        return Path + OSSeparator;
}

//-----------------------------------------------------------------------------
string CombinePathEx(string Part1, string Part2)
{
    string result;
    result = ExcludeTralingPathDelimiterEx(Part1) + OSSeparator +
        ExcludeLeadingPathDelimiterEx(Part2);
    return ExcludeTralingPathDelimiterEx(result);
}

//-----------------------------------------------------------------------------
string ExtractFilePathEx(string FileName)
{
    string::size_type LastPos;

    LastPos = FileName.find_last_of('/');
    if (LastPos == string::npos)
        LastPos = FileName.find_last_of('\\');

    if (LastPos != string::npos)
        return FileName.substr(0, LastPos);
    else
        return "";
}

//-----------------------------------------------------------------------------
string ExtractFileNameEx(string FileName)
{
    string::size_type LastPos;

    LastPos = FileName.find_last_of('/');
    if (LastPos == string::npos)
        LastPos = FileName.find_last_of('\\');

    if (LastPos == string::npos)
        return FileName;
    else
        return FileName.substr(LastPos + 1);
}

//-----------------------------------------------------------------------------
void SplitPathWithMaskEx(string PathWithMask, string &Dir, string &Mask)
{
    if ((PathWithMask.find('*') != string::npos) || (PathWithMask.find('?') != string::npos))
    {
        Dir = ExtractFilePathEx(PathWithMask);
        Mask = ExtractFileNameEx(PathWithMask);
    }
    else
    {
        Dir = PathWithMask;
        Mask = "";
    }
}

const char g_DriveDelimiter = ':';

//-----------------------------------------------------------------------------
string OSPathToVaultPath(string OSPath)
{
    string result = OSPath;
    if ((OSPath.length() > 2) && (OSPath[1] == g_DriveDelimiter))
    {
        result = result.substr(2);
    }
    while (true)
    {
        if ((result.find("./") == 0) || (result.find(".\\") == 0))
            result = result.substr(2);
        else if ((result.find("../") == 0) || (result.find("..\\") == 0))
            result = result.substr(3);
        else
            break;

    }
    if ((result == ".") || (result == ".."))
        result = string() + OSSeparator;
    else if ((result[0] != '/') && (result[0] != '\\'))
        result.insert(0, string() + OSSeparator);
    if (g_OptionUpperCase)
        transform(result.begin(), result.end(), result.begin(), ::toupper);
    if (g_OptionLowerCase)
        transform(result.begin(), result.end(), result.begin(), ::tolower);
    return result;
}


//-----------------------------------------------------------------------------
bool FileExists(string FileName)
{
    return (access(FileName.c_str(), 0) == 0);
}

//-----------------------------------------------------------------------------
bool DirectoryExists(string DirectoryName)
{
    struct stat st;
    return ((stat(ExcludeTralingPathDelimiterEx(DirectoryName).c_str(), &st) == 0) &&
        ((st.st_mode & S_IFDIR) != 0));
}

//-----------------------------------------------------------------------------
int ForceCreateDirectory(string DirectoryName)
{
    int Pos = 0;
    int result = 0;
    string SubDir;

    DirectoryName = IncludeTralingPathDelimiterEx(DirectoryName); // to create the dir itself on last iteration
    while ((Pos = (int)DirectoryName.find(OSSeparator, Pos + 1)) >= 0)
    {
        SubDir = DirectoryName.substr(0, Pos);
#ifdef _WIN32
        result = mkdir(SubDir.c_str());
#else
        result = mkdir(SubDir.c_str(), 0777);
#endif
        if (result == -1)
        {
            result = errno;
            if (result != EEXIST)
                return result;
            else
                result = 0;
        }
    }
    return result;
}

comp_string StdStringToComponentString(std::string &value)
{    
#ifdef _UNICODE
    const char* pValue = value.c_str();
    wchar_t *rValue = NULL;

    int Size = (int)value.length();
    
    if (Size == 0)
    {
        rValue = (wchar_t *)malloc(sizeof(WCHAR));
        rValue[0] = '\0';
    }
    else
    {
        int Sz = MultiByteToWideChar(CP_OEMCP, 0, pValue, Size, NULL, 0);
        rValue = (wchar_t *) malloc((Sz + 1) * sizeof(WCHAR));
        MultiByteToWideChar(CP_OEMCP, 0, pValue, Size, rValue, Sz);
        rValue[Sz] = '\0';
    }

    comp_string result(rValue);
    free(rValue);
    return *(new comp_string(result));    
#else
    return *(new comp_string(value));
#endif
}

std::string ComponentStringToStdString(
#ifdef _UNICODE
    LPCWSTR value
#else
    const char* value
#endif
)
{
    if (value == NULL)
        return *(new std::string(NULL));

#ifdef _UNICODE
    int Sz;
    int Size = (int)wcslen(value);
    LPSTR tmpBuf;

    Sz = WideCharToMultiByte(CP_UTF8, 0, value, Size, NULL, 0, NULL, NULL);
    tmpBuf = (LPSTR)malloc(Sz);
    Sz = WideCharToMultiByte(CP_UTF8, 0, value, Size, tmpBuf, Sz, NULL, NULL);
    std::string result(tmpBuf, Sz);
    free(tmpBuf);
    return *(new std::string(result));
#else
    return *(new std::string(value));
#endif
}

std::string ComponentStringToStdString(comp_string &value)
{
    return ComponentStringToStdString(value.c_str());
}

int AddDirectory(string RootDir, string FileName, ::CBVault& Vault);

int IterateFilesWithRoot(string RootDir, string Path,
    string Mask, IterateFilesFunction IterateFunc,
    bool Recursive, ::CBVault &Vault)
{
    DIR *Dir;
    string FilePath;
    bool IsDirectory;
    int result = 0;
    struct dirent *Entry;
    bool ProceedFilePath = false;

    Dir = opendir(CombinePathEx(RootDir, Path).c_str());
    if (Dir == NULL)
        return errno;

    try
    {
        result = AddDirectory(RootDir, Path, Vault);
        if (result != 0)
            return result;

        while ((Entry = readdir(Dir)) != NULL)
        {
            if ((strcmp(Entry->d_name, ".") == 0) || (strcmp(Entry->d_name, "..") == 0))
                continue;

            FilePath = CombinePathEx(Path, Entry->d_name);
            IsDirectory = DirectoryExists(CombinePathEx(RootDir, FilePath.c_str()));
            if (IsDirectory)
            {
                if (Recursive)
                {
                    result = IterateFilesWithRoot(RootDir, FilePath, Mask,
                        IterateFunc, Recursive, Vault);
                    if (result != 0)
                    {
                        closedir(Dir);
                        return result;
                    }
                    result = IterateFunc(RootDir, FilePath, IsDirectory, Vault);
                    if (result != 0)
                        return result;
                }
            }
            else
            {
                if (!IsMatchMask(Entry->d_name, Mask.c_str()))
                    continue;

                result = IterateFunc(RootDir, FilePath, IsDirectory, Vault);
                if (result != 0)
                    return result;
            }
        }
    }
    catch (...)
    {
        closedir(Dir);
        return errno;
    }
    closedir(Dir);
    return 0;
}


//-----------------------------------------------------------------------------
int IterateFiles(string FilePath, IterateFilesFunction IterateFunc,
    bool Recursive, CBVault &Vault)
{
    bool IsDirectory;
    bool MaskSpecified;
    bool ProceedFilePath = false;
    string RootDir, Mask, FullPath;
    int result = 0;

    SplitPathWithMaskEx(FilePath, RootDir, Mask);
    if (RootDir.empty())
        RootDir = ".";
    MaskSpecified = (Mask.length() > 0);
    if (!MaskSpecified)
    {
        IsDirectory = DirectoryExists(RootDir.c_str());
        if (IsDirectory)
        {
            Mask = "*";
            MaskSpecified = true;
        }
        ProceedFilePath = !IsDirectory;
    }

    if (MaskSpecified)
    {
        result = IterateFilesWithRoot(RootDir, string() + OSSeparator, Mask,
            IterateFunc, Recursive, Vault);        
    }

    if ((result == 0) && ProceedFilePath)
    {
        result = IterateFunc(".", FilePath, false, Vault);
    }
    return result;
}

//-----------------------------------------------------------------------------
int IterateVault(string FilePath, IterateVaultFunction IterateFunc, bool ProcessSubfolders, ::CBVault &Vault)
{
    int64 SearchData;
    string SearchTemplate;
    string RootDir, Mask;
    bool MaskSpecified;
    bool ProceedFilePath = false;
    int result = 0;

    
    SplitPathWithMaskEx(FilePath, RootDir, Mask);
    MaskSpecified = (Mask.length() > 0);
    if (!MaskSpecified)
    {
        if ((Vault.GetFileAttributes(StdStringToComponentString(FilePath).c_str()) & cbvConstants::VAULT_FATTR_DIRECTORY) == cbvConstants::VAULT_FATTR_DIRECTORY)
        {
            Mask = "*";
            ProceedFilePath = true;
        }
    }

    SearchTemplate = CombinePathEx(RootDir, Mask);
    SearchData = Vault.FindFirst(StdStringToComponentString(SearchTemplate).c_str(), cbvConstants::VAULT_FATTR_ANY_FILE, 0);
    if (SearchData != -1)
    {
        comp_string fullName;
        string sFullName;
        string combinedPath;
        do
        {
            IterateFunc(SearchData, Vault);
            if (((Vault.GetSearchResultAttributes(SearchData) & cbvConstants::VAULT_FATTR_DIRECTORY) == cbvConstants::VAULT_FATTR_DIRECTORY) && ProcessSubfolders)
            {
                fullName = Vault.GetSearchResultFullName(SearchData);
                sFullName = ComponentStringToStdString(fullName);
                combinedPath = CombinePathEx(sFullName, Mask);
                result = IterateVault(combinedPath,
                    IterateFunc, ProcessSubfolders, Vault);
                if (result != 0 && result != cbvConstants::VAULT_ERR_FILE_NOT_FOUND)
                {
                    Vault.FindClose(SearchData);
                    return result;
                }
            }
        } while (Vault.FindNext(SearchData));
        
        result = Vault.GetLastErrorCode();
        Vault.FindClose(SearchData);
        
        if (result != 0 && result != cbvConstants::VAULT_ERR_FILE_NOT_FOUND && result != cbvConstants::VAULT_ERR_NO_MORE_FILES)
            return result;
    }
    else
    {
        result = Vault.GetLastErrorCode(); 
        if (result != 0 && result != cbvConstants::VAULT_ERR_FILE_NOT_FOUND)
            return result;
        else
            result = 0;
    }

    if (ProceedFilePath)
    {
        SearchData = Vault.FindFirst(StdStringToComponentString(FilePath).c_str(), cbvConstants::VAULT_FATTR_ANY_FILE, 0);
        if (SearchData != -1)
        {
            result = IterateFunc(SearchData, Vault);
            Vault.FindClose(SearchData);
        }
        else
        {
            result = Vault.GetLastErrorCode();
            if (result != 0 && result != cbvConstants::VAULT_ERR_FILE_NOT_FOUND)
                return result;
            else
                result = 0;
        }
    }

    return result;
}

//-----------------------------------------------------------------------------
int OpenVault(string VaultName, ::CBVault *Vault, bool ExistingOnly)
{
    int result = 0;
    Vault->SetVaultFile(StdStringToComponentString(VaultName).c_str());

    if (ExistingOnly)
    {
        result = Vault->OpenVault(cbvConstants::VAULT_OM_OPEN_EXISTING, cbvConstants::VAULT_JM_METADATA);
    }
    else
    if (g_OptionOverwriteVault || !FileExists(VaultName))
    {
        if (g_OptionPageSize != 0)
        {
            Vault->SetPageSize(g_OptionPageSize);
        }

        if (g_OptionFixedVaultSize != 0)
        {
            Vault->SetVaultSizeMax(g_OptionFixedVaultSize);
            Vault->SetVaultSizeMin(g_OptionFixedVaultSize);
        }
        result = Vault->OpenVault(cbvConstants::VAULT_OM_CREATE_ALWAYS, cbvConstants::VAULT_JM_METADATA);
    }
    else
    {
        result = Vault->OpenVault(cbvConstants::VAULT_OM_OPEN_ALWAYS, cbvConstants::VAULT_JM_METADATA);
    }

    if (result != 0)
        cout << "Error opening vault: " << Vault->GetLastError() << endl;

    return result;

}

//-----------------------------------------------------------------------------

int AddDirectory(string RootDir, string FileName, ::CBVault& Vault)
{
    int result = 0;
    string VaultFilePath = OSPathToVaultPath(CombinePathEx(RootDir, FileName).c_str());

    if (!Vault.FileExists(StdStringToComponentString(VaultFilePath).c_str()))
    {
        cout << "Adding dir  " << VaultFilePath;
        result = Vault.CreateDirectory(StdStringToComponentString(VaultFilePath).c_str(), true);
        if (result == 0)
            cout << " - ok" << endl;
        else
            cout << " - error: " << Vault.GetLastError() << endl;
    }
    return result;
}

//-----------------------------------------------------------------------------
int AddFile(string RootDir, string FileName, CBVault &Vault)
{
    int result = 0;
    struct stat OsFileStat;
    string VaultFileName, VaultFilePath, OSFileName;

    OSFileName = CombinePathEx(RootDir, FileName).c_str();
    VaultFileName = OSPathToVaultPath(OSFileName);

    if (stat(OSFileName.c_str(), &OsFileStat) != 0)
    {
        result = errno;
        cout << "Adding file " << VaultFileName << " - error: " << strerror(result) << endl;
    }

    if (result == 0)
    {
        if (Vault.FileExists(StdStringToComponentString(VaultFileName).c_str()))
        {
            bool AddFile;
            if (g_OptionAlwaysYes)
                AddFile = true;
            else
            {
                if (g__OptionOverwriteFilesSet)
                {
                    if (g_OptionOverwriteFiles)
                        AddFile = true;
                    else
                    {
                        cout << "File " << VaultFileName << " already exists. Skipped" << endl;
                        AddFile = false;
                    }
                }
                else
                {
                    stringstream Question;
                    Question << "File " << VaultFileName << " already exists. Overwrite";
                    switch (AskYNA(Question.str()))
                    {
                    case ANSWER_YES:
                        AddFile = true;
                        break;
                    case ANSWER_ALL:
                        AddFile = true;
                        g__OptionOverwriteFilesSet = true;
                        g_OptionOverwriteFiles = true;
                        break;
                    default:  //ANSWER_UNKNOWN, ANSWER_NO
                        AddFile = false;
                        cout << "Skipped" << endl;
                        break;
                    }
                }
            }
            if (!AddFile)
                return result;
            result = Vault.DeleteFile(StdStringToComponentString(VaultFileName).c_str());
            if (result != 0)
            {
                cout << " - error: " << Vault.GetLastError() << endl;
            }
        }

        if (result == 0)
        {
            AddDirectory(RootDir, ExtractFilePathEx(FileName), Vault);

            cout << "Adding file " << VaultFileName;

            ifstream FileStream(OSFileName, ios::in | ios::binary);

            if (result == 0)
            {
                ::CBFSVaultStream* VaultStream = Vault.OpenFileEx(StdStringToComponentString(VaultFileName).c_str(), cbvConstants::VAULT_FOM_CREATE_NEW, false, true, true, true,
                    (g_OptionPassword == "") ? cbvConstants::VAULT_EM_NONE : cbvConstants::VAULT_EM_DEFAULT,
                    StdStringToComponentString(g_OptionPassword).c_str(),
                    g_OptionCompressionLevel == -1 ? cbvConstants::VAULT_CM_NONE : cbvConstants::VAULT_CM_ZLIB,
                    g_OptionCompressionLevel, 0);
                if (VaultStream == NULL)
                {
                    result = Vault.GetLastErrorCode();
                    cout << " - error: " << Vault.GetLastError() << endl;
                }
                else
                {

                    vector<char> Buffer(BUFFER_SIZE);
                    int BytesRead, BytesWritten;
                    while (true)
                    {
                        FileStream.read(&Buffer[0], BUFFER_SIZE);
                        BytesRead = (int)FileStream.gcount();
                        if (BytesRead > 0)
                        {
                            BytesWritten = VaultStream->Write(&Buffer[0], BytesRead);
                            if (BytesWritten != BytesRead)
                            {
                                result = VaultStream->GetLastErrorCode();
                                if (result != 0)
                                {
                                    cout << " - error: " << VaultStream->GetLastError() << endl;
                                    break;
                                }
                            }
                        }
                        else
                            break;
                    }
                }

                if (result == 0)
                    cout << " - ok" << endl;

                if (VaultStream != NULL)
                {
                    VaultStream->Close();
                    delete VaultStream;
                }
                FileStream.close();
            }
        }
    }
    
    return result;
}

//-----------------------------------------------------------------------------
int AddFileObject(string RootDir, string FilePath, bool IsDirectory, ::CBVault& Vault)
{
    int result = 0;
    string OSFileName = CombinePathEx(RootDir, FilePath).c_str();

    if (IsDirectory)
    {
        if (g_OptionDeleteAfterArchiving)
        {
            cout << "Deleting dir  " << OSFileName;
            if (rmdir(OSFileName.c_str()) != 0)
            {
                result = errno;
                cout << " - error: " << strerror(result) << endl;
            }
            else
                cout << " - ok" << endl;
        }
    }
    else
    {
        result = AddFile(RootDir, FilePath, Vault);
        if ((result == 0) && g_OptionDeleteAfterArchiving)
        {
            cout << "Deleting file " << OSFileName;
            if (unlink(OSFileName.c_str()) != 0)
            {
                result = errno;
                cout << " - error: " << strerror(result) << endl;
            }
            else
                cout << " - ok" << endl;
        }
    }

    return result;
}

//-----------------------------------------------------------------------------
int DeleteFileObject(int64 SearchData, ::CBVault &Vault)
{
    int result = 0;
    comp_string fullName = Vault.GetSearchResultFullName(SearchData);

    result = Vault.GetLastErrorCode();

    if (result == 0)
    {
        if ((Vault.GetSearchResultAttributes(SearchData) & cbvConstants::VAULT_FATTR_DIRECTORY) == cbvConstants::VAULT_FATTR_DIRECTORY)
            cout << "Deleting directory " << ComponentStringToStdString(fullName);
        else
            cout << "Deleting file " << ComponentStringToStdString(fullName);
        result = Vault.DeleteFile(fullName.c_str());
    }
    if (result != 0)
        cout << " - error: " << Vault.GetLastError() << endl;
    else
        cout << " - ok" << endl;

    return result;
}

//-----------------------------------------------------------------------------
int ExtractFile(int64 SearchData, ::CBVault& Vault)
{
    int result = 0;

    string OSFileName;

    if (g_Command == Extract)
    {
        OSFileName = ComponentStringToStdString(Vault.GetSearchResultName(SearchData));
    }
    else
    if (g_Command == eXtract)
    {
        OSFileName = ComponentStringToStdString(Vault.GetSearchResultFullName(SearchData));
        OSFileName = ExcludeLeadingPathDelimiterEx(OSFileName);
        //replace(OSFileName.begin(), OSFileName.end(), Vault.get_PathSeparator());
    }
    else
    {
        return 1;
    }

    result = Vault.GetLastErrorCode();
    if (result != 0)
    {
        return result; 
    }

    OSFileName = CombinePathEx(g__OutputPath, OSFileName);
    if (FileExists(OSFileName))
    {
        bool ExtractThisFile;
        if (g_OptionAlwaysYes)
        {
            ExtractThisFile = true;
        }
        else if (g__OptionOverwriteFilesSet)
        {
            if (g_OptionOverwriteFiles)
            {
                ExtractThisFile = true;
            }
            else
            {
                ExtractThisFile = false;
                cout << "File " << OSFileName << " already exists. Skipped" << endl;
            }
        }
        else
        {
            stringstream Question;
            Question << "File " << OSFileName << " already exists. Overwrite";
            switch (AskYNA(Question.str()))
            {
            case ANSWER_YES:
                ExtractThisFile = true;
                break;
            case ANSWER_ALL:
                ExtractThisFile = true;
                g__OptionOverwriteFilesSet = true;
                g_OptionOverwriteFiles = true;
                break;
            default:  //ANSWER_UNKNOWN, ANSWER_NO
                ExtractThisFile = false;
                cout << "Skipped" << endl;
                break;
            }

        }
        if (!ExtractThisFile)
            return 0;
    }

    string OSPath = ExtractFilePathEx(OSFileName);

    result = 0;

    if (OSPath != "")
        result = ForceCreateDirectory(OSPath);

    if (result != 0)
    {
        cout << " - error: " << result << endl;
    }
    else
    {
        comp_string fullName = Vault.GetSearchResultFullName(SearchData);

        cout << "Extracting file " << ComponentStringToStdString(fullName).c_str();
    
        ofstream FileStream(OSFileName.c_str(), ios::out | ios::binary);

        char* LastErrorMsg = NULL;

        CBFSVaultStream *VaultStream = Vault.OpenFile(fullName.c_str(),
            cbvConstants::VAULT_FOM_OPEN_EXISTING, true, false, 
            StdStringToComponentString(g_OptionPassword).c_str());

        if (VaultStream == NULL)
        {
            result = Vault.GetLastErrorCode();
            LastErrorMsg = Vault.GetLastError();
        }
        else
        {
            vector<char> Buffer(BUFFER_SIZE);
            long TotalRead = 0;
            int BytesRead;
            while (TotalRead < VaultStream->GetLength())
            {
                BytesRead = VaultStream->Read(&Buffer[0], BUFFER_SIZE);
                if (VaultStream->GetLastErrorCode() != 0)
                {
                    result = VaultStream->GetLastErrorCode();
                    LastErrorMsg = VaultStream->GetLastError();
                    break;
                }
                TotalRead += BytesRead;
                FileStream.write(&Buffer[0], BytesRead);
            }
            VaultStream->Close();
            delete VaultStream;

            cout << " - ok" << endl;
        }

        FileStream.close();        

        if (result != 0)
        {
            cout << " - error: " << LastErrorMsg << endl;
        }
    }    
    return result;
}

//-----------------------------------------------------------------------------
int ExtractDirectory(int64 SearchData, CBVault &Vault)
{
    int result = 0;

    if (g_Command == Extract)
        return 0;

    string fullName = ComponentStringToStdString(Vault.GetSearchResultFullName(SearchData));
    
    string DirectoryName = ExcludeLeadingPathDelimiterEx(fullName);
    //replace(DirectoryName.begin(), DirectoryName.end(), Vault.get_PathSeparator(), '/');
    DirectoryName = CombinePathEx(g__OutputPath, DirectoryName);
    cout << "Extracting directory " << fullName;
    result = ForceCreateDirectory(DirectoryName);

    if (result != 0)
    {
        cout << " - error: " << result << endl;
    }
    else
        cout << " - ok" << endl;
    return result; 
}

//-----------------------------------------------------------------------------
int ExtractFileObject(int64 SearchData, CBVault &Vault)
{
    int result;

    if ((Vault.GetSearchResultAttributes(SearchData) & cbvConstants::VAULT_FATTR_DIRECTORY) == cbvConstants::VAULT_FATTR_DIRECTORY)
        result = ExtractDirectory(SearchData, Vault);
    else
        result = ExtractFile(SearchData, Vault);

    return result;
}

//-----------------------------------------------------------------------------
int ListFile(int64 SearchData, ::CBVault &Vault)
{

    string fullName = ::ComponentStringToStdString(Vault.GetSearchResultFullName(SearchData));
    
    cout.width(68); cout << left << fullName;

    if ((Vault.GetSearchResultAttributes(SearchData) & cbvConstants::VAULT_FATTR_DIRECTORY) == cbvConstants::VAULT_FATTR_DIRECTORY)
    {
        cout.width(11); cout << right << "<dir>";
    }
    else
    {
        cout.width(11); cout << right << Vault.GetSearchResultSize(SearchData);
    }
    //cout.width(20); cout << right << ctime((const time_t *)&SearchData.Modification);
    cout << endl;

    return 0;
}


//-----------------------------------------------------------------------------
int TestFile(int64 SearchData, ::CBVault &Vault)
{
    int result = 0;

    if ((Vault.GetSearchResultAttributes(SearchData) & cbvConstants::VAULT_FATTR_DIRECTORY) == cbvConstants::VAULT_FATTR_DIRECTORY)
        return 0;

    comp_string fullName = Vault.GetSearchResultFullName(SearchData);

    cout << "Testing file " << ComponentStringToStdString(fullName);

    char* LastErrorMsg = NULL;

    CBFSVaultStream *VaultStream = Vault.OpenFile(fullName.c_str(), 
        cbvConstants::VAULT_FOM_OPEN_EXISTING, true, false, 
        StdStringToComponentString(g_OptionPassword).c_str());

    if (VaultStream == NULL)
    {
        result = Vault.GetLastErrorCode();
        LastErrorMsg = Vault.GetLastError();
    }
    else
    {
        vector<char> Buffer(BUFFER_SIZE);
        long TotalRead = 0;
        int BytesRead;
        while (TotalRead < VaultStream->GetLength())
        {
            BytesRead = VaultStream->Read(&Buffer[0], BUFFER_SIZE);            
            result = VaultStream->GetLastErrorCode();
            if (result != 0)
            {
                LastErrorMsg = VaultStream->GetLastError();
                break;
            }
            TotalRead += BytesRead;
        }
        VaultStream->Close();        
    }
    if (result != 0)
        cout << " - error: " << LastErrorMsg << endl;
    else
        cout << " - ok" << endl;

    return result;
}


//-----------------------------------------------------------------------------
int AddFiles(string VaultName, vector<string> FileNames)
{
    int result = 0;

    CBVault Vault;

    result = OpenVault(VaultName, &Vault, false);
    if (result != 0)
        return result;

    for (unsigned int i = 0; i < FileNames.size(); i++)
    {
        result = IterateFiles(FileNames.at(i), AddFileObject, g_OptionRecursive, Vault);
        if (result != 0)
            break;
    }
    return result;

}

//-----------------------------------------------------------------------------
int DeleteFiles(string VaultName, vector<string> FileNames)
{
    int result = 0;

    CBVault Vault;

    result = OpenVault(VaultName, &Vault, true);
    if (result != 0)
        return result; 

    for (unsigned int i = 0; i < FileNames.size(); i++)
    {
        result = IterateVault(FileNames.at(i), DeleteFileObject, true, Vault);
        if (result != 0)
            break;
    }
    return result;
}

//-----------------------------------------------------------------------------
int ExtractFiles(string VaultName, vector<string> FileNames)
{
    int result = 0;

    CBVault Vault;
    
    result = OpenVault(VaultName, &Vault, true);
    if (result != 0)
        return result;

    if (FileNames.size() == 0)
        FileNames.push_back("*");

    for (unsigned int i = 0; i < FileNames.size(); i++)
    {
        result = IterateVault(FileNames.at(i), ExtractFileObject, true, Vault);
        if (result != 0)
            break;
    }
    return result;
}

//-----------------------------------------------------------------------------
int ListFiles(string VaultName, vector<string> FileNames)
{
    int result = 0;
    
    CBVault Vault;

    result = OpenVault(VaultName, &Vault, true);
    if (result != 0)
        return result;

    if (FileNames.size() == 0)
        FileNames.push_back("*");

    cout.width(68); cout << left << "File Name"; cout.width(11); cout << right << "Size" << endl;
    cout << string(79, '-') << endl;

    std::string fileName; 

    for (unsigned int i = 0; i < FileNames.size(); i++)
    {
        fileName = FileNames.at(i);
        result = IterateVault(fileName, ListFile, true, Vault);
        if (result != 0)
            break;
    }
    return result;    
}


//-----------------------------------------------------------------------------
int TestFiles(string VaultName, vector<string> FileNames)
{
    int result = 0;

    CBVault Vault;

    result = OpenVault(VaultName, &Vault, true);
    if (result != 0)
        return result;

    if (FileNames.size() == 0)
        FileNames.push_back("*");

    for (unsigned int i = 0; i < FileNames.size(); i++)
    {
        result = IterateVault(FileNames.at(i), TestFile, true, Vault);
        if (result != 0)
            break;
    }
    return result;
}

//-----------------------------------------------------------------------------
int main(int argc, char* argv[])
{
    char InputCommand;
    char Option;
    string Parameter;
    string VaultName;
    vector<string> FileNames;
    CommandFunction CmdFunc;
    int ParNo;
    bool SkipOptions;
    int result = 0;

    if ((argc < 3) || (strlen(argv[1]) != 1))
    {
        Usage();
        return 0;
    }

    InputCommand = tolower(argv[1][0]);

    if (AllCommands.find(InputCommand) == string::npos)
    {
        cout << "Invalid command \"" << InputCommand << "\"" << endl;
        return -1;
    }

    ParNo = 2;
    SkipOptions = false;
    while (ParNo < argc)
    {
        Parameter = argv[ParNo];
        if ((Parameter.length() < 2) || (Parameter[0] != '-'))
            break;

        Option = tolower(Parameter[1]);
        if (Option == '-')
            SkipOptions = true;
        if (SkipOptions)
        {
            ParNo++;
            continue;
        }

        if (AllOptions.find(Option) == string::npos)
        {
            cout << "Invalid option \"" << Option << "\"" << endl;
            return -1;
        }

        if (AllOptionsWithParameters.find(Option) != string::npos)
        {
            if ((Parameter.length() < 3) || (!SetOption(Option, Parameter.substr(2))))
            {
                cout << "Invalid option \"" << Option << "\"" << endl;
                return -1;
            }
        }
        else
        {
            if (!SetOption(Option, ""))
            {
                cout << "Invalid option \"" << Option << "\"" << endl;
                return -1;
            }
        }

        ParNo++;
    }

    if (ParNo >= argc)
    {
        Usage();
        return 0;
    }

    VaultName = argv[ParNo++];

    for (int i = ParNo; i < argc; i++)
    {
        FileNames.push_back(argv[i]);
    }

    switch (InputCommand)
    {
    case 'a':
        g_Command = Add;
        CmdFunc = AddFiles;
        break;
    case 'd':
        g_Command = Delete;
        CmdFunc = DeleteFiles;
        break;
    case 'e':
        g_Command = Extract;
        CmdFunc = ExtractFiles;
        break;
    case 'l':
        g_Command = List;
        CmdFunc = ListFiles;
        break;
    case 't':
        g_Command = Test;
        CmdFunc = TestFiles;
        break;
    case 'x':
        g_Command = eXtract;
        CmdFunc = ExtractFiles;
        break;
    default:
        cout << "Invalid command \"" << InputCommand << "\"" << endl;
        return -1;
    }

    if ((g_Command == Extract) || (g_Command == eXtract))
    {
        char Buffer[1024];
        if (g_OptionOutputPath == "")
        {
            g__OutputPath = getcwd(Buffer, sizeof(Buffer) / sizeof(char) - 1);
            //if (GetCurrentDirectory(MAX_PATH, Buffer))
            //    g__OutputPath = Buffer;
        }
        else
        {
#ifdef _WIN32
            if (_fullpath(Buffer, g_OptionOutputPath.c_str(), sizeof(Buffer) / sizeof(char) - 1))
                g__OutputPath = Buffer;
#else
            g__OutputPath = realpath(g_OptionOutputPath.c_str(), Buffer);
#endif
        }
    }

    result = CmdFunc(VaultName, FileNames);

    return result;
}

 
 
 
 

