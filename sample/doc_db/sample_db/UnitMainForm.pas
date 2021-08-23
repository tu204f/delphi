unit UnitMainForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls;

type
  TMainForm = class(TForm)
    ListBoxFolder: TListBox;
    ButtonOpenDB: TButton;
    ButtonCloseDB: TButton;
    ListBoxDoc: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    ButtonAddFolder: TButton;
    ButtonRenameFolder: TButton;
    ButtonDeletedFolder: TButton;
    ButtonOpenFolder: TButton;
    LabelPath: TLabel;
    ButtonFolderRedo: TButton;
    LabelPathFolderID: TLabel;
    ButtonNewDocument: TButton;
    ButtonOpenDocument: TButton;
    ButtonCloseDocument: TButton;
    ButtonDeleteDocument: TButton;
    procedure ButtonOpenDBClick(Sender: TObject);
    procedure ButtonCloseDBClick(Sender: TObject);
    procedure ButtonAddFolderClick(Sender: TObject);
    procedure ButtonRenameFolderClick(Sender: TObject);
    procedure ButtonDeletedFolderClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListBoxFolderClick(Sender: TObject);
    procedure ButtonOpenFolderClick(Sender: TObject);
    procedure ButtonFolderRedoClick(Sender: TObject);
    procedure ButtonNewDocumentClick(Sender: TObject);
    procedure ButtonOpenDocumentClick(Sender: TObject);
    procedure ButtonCloseDocumentClick(Sender: TObject);
    procedure ButtonDeleteDocumentClick(Sender: TObject);
  private
    FSelectedFolderID: String;
    FParentFoldersID: TStrings;
  public
    { Public declarations }
    procedure SetSelected(const AFolderID: String);
    procedure SetSelectedDocument(const AFolderID: String);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Lb.Doc.DB,
  UnitValueForm,
  UnitDocumentForm;

var
  localDataBase: TDataBase;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(FParentFoldersID);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FSelectedFolderID := 'root';
  FParentFoldersID := TStringList.Create;
end;

procedure TMainForm.ListBoxFolderClick(Sender: TObject);
var
  xIndex: Integer;
begin
  xIndex := ListBoxFolder.ItemIndex;
  if xIndex >= 0 then
  begin
    FSelectedFolderID := FParentFoldersID[xIndex];
    SetSelectedDocument(FSelectedFolderID);
  end;
end;

procedure TMainForm.ButtonOpenDBClick(Sender: TObject);
begin
  // открыть базу даннух
  localDataBase := TDocFile.GetOpen;
  FSelectedFolderID := 'root';
  SetSelected(FSelectedFolderID);
  SetSelectedDocument(FSelectedFolderID);
end;

procedure TMainForm.ButtonCloseDBClick(Sender: TObject);
begin
  // Закрыть базу данных
  TDocFile.SetClose(localDataBase);
end;

procedure TMainForm.SetSelected(const AFolderID: String);
var
  xFolder: TFolder;
  xFolders: TFolders;
begin
  // Получить список папок
  ListBoxFolder.Clear;
  FParentFoldersID.Clear;
  xFolders := TFolders.Create(localDataBase);
  try
    xFolders.ParentFolderID := AFolderID;
    for xFolder in xFolders.Items do
    begin
      FParentFoldersID.Add(xFolder.ID);
      ListBoxFolder.Items.Add(xFolder.Name);
    end;
    LabelPath.Caption := xFolders.Path;
    LabelPathFolderID.Caption := xFolders.PathFolderID;
  finally
    FreeAndNil(xFolders);
  end;
end;

procedure TMainForm.SetSelectedDocument(const AFolderID: String);
var
  xS: String;
  xFolder: TFolder;
  xDocument: TDocument;
  i, Count: Integer;
begin
  ListBoxDoc.Clear;
  xFolder := TFolder.Create(localDataBase);
  try
    xFolder.ID := AFolderID;
    Count := xFolder.Documents.Items.Count;
    if Count > 0 then
      for i := 0 to Count - 1 do
      begin
        xDocument := xFolder.Documents.Items[i];
        xS := xDocument.ID;
        ListBoxDoc.Items.Add(xS);
      end;
  finally
    FreeAndNil(xFolder);
  end;
end;

// *****************************************************************************
// Работа с папками
// *****************************************************************************

procedure TMainForm.ButtonAddFolderClick(Sender: TObject);
var
  xS: String;
  xID: String;
  xFolders: TFolders;
begin
  // добавить папку
  xS := GetValueForm('Добавить папку');
  if not xS.IsEmpty then
  begin
    xFolders := TFolders.Create(localDataBase);
    try
      xFolders.ParentFolderID := FSelectedFolderID;
      xID := xFolders.AddFolder(xS);
      if not xID.IsEmpty then
      begin
        ListBoxFolder.Items.Add(xS);
        FParentFoldersID.Add(xID);
      end;
    finally
      FreeAndNil(xFolders);
    end;
  end;
end;

procedure TMainForm.ButtonRenameFolderClick(Sender: TObject);
var
  xS: String;
  xID: String;
  xFolders: TFolders;
var
  xIndex: Integer;
begin
  // Переменовать папку
  xIndex := ListBoxFolder.ItemIndex;
  if xIndex >= 0 then
  begin
    xS := GetValueForm('Добавить папку','Редактирование папки');
    if not xS.IsEmpty then
    begin
      xFolders := TFolders.Create(localDataBase);
      try
        xFolders.ParentFolderID := FSelectedFolderID;
        xID := FParentFoldersID[xIndex];
        if xFolders.RenameFolder(xID,xS) then
          ListBoxFolder.Items[xIndex] := xS;
      finally
        FreeAndNil(xFolders);
      end;
    end;
  end;
end;

procedure TMainForm.ButtonDeletedFolderClick(Sender: TObject);
var
  xID: String;
  xFolders: TFolders;
var
  xIndex: Integer;
begin
  // Удалить папку
  xIndex := ListBoxFolder.ItemIndex;
  if xIndex >= 0 then
  begin
    xFolders := TFolders.Create(localDataBase);
    try
      xFolders.ParentFolderID := FSelectedFolderID;
      xID := FParentFoldersID[xIndex];
      if xFolders.DeleteFolder(xID) then
        ListBoxFolder.Items.Delete(xIndex);
    finally
      FreeAndNil(xFolders);
    end;
  end;
end;

// *****************************************************************************
// Открыть папку, и получить список документов и папку
// *****************************************************************************

procedure TMainForm.ButtonOpenFolderClick(Sender: TObject);
var
  xID: String;
  xFolders: TFolders;
var
  xIndex: Integer;
begin
  // Открыть папку
  xIndex := ListBoxFolder.ItemIndex;
  if xIndex >= 0 then
  begin
    xFolders := TFolders.Create(localDataBase);
    try
      xFolders.ParentFolderID := FSelectedFolderID;
      xID := FParentFoldersID[xIndex];
      SetSelected(xID);
    finally
      FreeAndNil(xFolders);
    end;
  end;
end;

procedure TMainForm.ButtonFolderRedoClick(Sender: TObject);
var
  xSelectedFolderID: String;
  xFolders: TFolders;
begin
  xFolders := TFolders.Create(localDataBase);
  try
    xFolders.ParentFolderID := FSelectedFolderID;
    xSelectedFolderID := xFolders.ParentToFolderID;
    if not xSelectedFolderID.IsEmpty then
    begin
      FSelectedFolderID := xSelectedFolderID;
      SetSelected(FSelectedFolderID);
      SetSelectedDocument(FSelectedFolderID);
    end;
  finally
    FreeAndNil(xFolders);
  end;
end;

// *****************************************************************************
//
// *****************************************************************************

procedure TMainForm.ButtonNewDocumentClick(Sender: TObject);
var
  xFolder: TFolder;
begin
  xFolder := TFolder.Create(localDataBase);
  try
    xFolder.ID := FSelectedFolderID;
    xFolder.NewDocument;
    SetSelectedDocument(FSelectedFolderID);
  finally
    FreeAndNil(xFolder);
  end;
end;

procedure TMainForm.ButtonOpenDocumentClick(Sender: TObject);
var
  xIndex: Integer;
  xFolder: TFolder;
  xDocument: TDocument;
begin
  //  Открыть документа
  xIndex := ListBoxDoc.ItemIndex;
  if xIndex >= 0 then
  begin
    xFolder := TFolder.Create(localDataBase);
    try
      xFolder.ID := FSelectedFolderID;
      xDocument := xFolder.Documents.Items[xIndex];
      SetDocumentForm(xDocument);
    finally
      FreeAndNil(xFolder);
    end;
  end;
end;

procedure TMainForm.ButtonCloseDocumentClick(Sender: TObject);
begin
  // Зарыкть документ
end;

procedure TMainForm.ButtonDeleteDocumentClick(Sender: TObject);
begin
  // Удалить документ
end;

end.
