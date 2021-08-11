unit UnitMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
  TMainForm = class(TForm)
    ButtonLoad: TButton;
    ButtonCreateFolder: TButton;
    procedure ButtonLoadClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonCreateFolderClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  Lb.Doc.DB,
  Lb.Candel.Source,
  Lb.Candel.SysUtils;


var
  localDataBase: TDataBase;
  localSelectedFolderID: String;
  localCandels: TSourceCandel = nil;


///<summary>Добавить новую папку</summary>
///<remarks>
/// ANewFolderName - Имя паки
/// AParentFolderID - где будет создовать папка
///</remarks>
function GetAddFolder(const ANewFolderName, AParentFolderID: String): String;
var
  xID: String;
  xFolders: TFolders;
begin
  if not ANewFolderName.IsEmpty then
  begin
    xFolders := TFolders.Create(localDataBase);
    try
      xFolders.ParentFolderID := AParentFolderID;
      xID := xFolders.AddFolder(ANewFolderName);
      Result := xID;
    finally
      FreeAndNil(xFolders);
    end;
  end;
end;


procedure TMainForm.ButtonCreateFolderClick(Sender: TObject);
begin
  GetAddFolder('candels','root');
  GetAddFolder('vectors','root');
end;

procedure TMainForm.ButtonLoadClick(Sender: TObject);
var
  xCandel: TCandel;
begin
  var xStr := TStringList.Create;
  try
    // ----------------------------------
    xStr.LoadFromFile('export.csv');
    // ----------------------------------
    localCandels.SetParserCandels(xStr);
    for xCandel in localCandels.Candels do
    begin

    end;
    // ----------------------------------
  finally
    FreeAndNil(xStr);
  end;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  localDataBase := TDocFile.GetOpen;
  localSelectedFolderID := 'root';
  localCandels := TSourceCandel.Create;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TDocFile.SetClose(localDataBase);
  FreeAndNil(localCandels);
end;



end.
