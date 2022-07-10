unit UnitImportMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox,
  FMX.Memo;

type

  TOnProgerss = procedure(const AValue, AValueMax: Integer) of object;

  TMainForm = class(TForm)
    ImportButton: TButton;
    OpenDialog: TOpenDialog;
    MemoLog: TMemo;
    ProgressBar1: TProgressBar;
    procedure ImportButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CallBakProgerss(const AValue, AValueMax: Integer);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  Data.DB,
  Lb.Candel.SysUtils,
  Lb.DataModuleDB, Lb.SysUtils.ISO860;

var
  localDataModuleDB: TDataModuleDB = nil;

function GetDataModuleDB: TDataModuleDB;
begin
  if not Assigned(localDataModuleDB) then
  begin
    localDataModuleDB := TDataModuleDB.Create(nil);
    localDataModuleDB.DefaultConnection('candels.db');
  end;
  Result := localDataModuleDB;
end;

function GetIsSecurity(const AName: String): Boolean;
const
  SQL_IS_NAME = 'select count(*) from security where name = :name';
var
  xValue: Variant;
begin
  Result := False;
  xValue := GetDataModuleDB.GetExecSQLScalar(SQL_IS_NAME,[AName],[ftString]);
  if not VarIsNull(xValue) then
    Result := (xValue > 0);
end;

procedure SetDeleteSecurity(const AName: String);
const
  SQL_IS_NAME = 'delete from security where name = :name';
begin
  GetDataModuleDB.GetExecSQL(SQL_IS_NAME,[AName],[ftString]);
end;

function GetInsertSecurity(const AName: String): Integer;
const
  SQL_ROWID = 'select last_insert_rowid()';
  SQL_INSERT = 'insert into security (name) values(:name)';
var
  xValueRowID: Variant;
begin
  Result := -1;
  GetDataModuleDB.GetExecSQL(SQL_INSERT,[AName],[ftString]);
  xValueRowID := GetDataModuleDB.GetExecSQLScalar(SQL_ROWID);
  if not VarIsNull(xValueRowID) then
    Result := xValueRowID;
end;

procedure SetInsertCandel(const ASecurityID: Integer; const ACandel: TCandel);
const
  SQL_INSERT =
    'insert into candel(security_id,open,high,low,close,value,date,time) ' +
    'values(:security_id,:open,:high,:low,:close,:value,:date,:time)';
begin
  // Написать импорт текстового файла в базу
  GetDataModuleDB.GetExecSQL(SQL_INSERT,
    [
      ASecurityID,
      ACandel.Open,
      ACandel.High,
      ACandel.Low,
      ACandel.Close,
      Acandel.Vol,
      GetDateToStrISO860(ACandel.Date),
      GetTimeToStrISO860(ACandel.Time)
    ],
    [ftInteger,ftFloat,ftFloat,ftFloat,ftFloat,ftFloat,ftString,ftString]
  );
end;

procedure SetImport(const ANameCandel: String; AFileName: String; ACallBakProgerss: TOnProgerss = nil);
var
  xS: String;
  i, iCount: Integer;
  xStr: TStrings;
  xCandel: TCandel;
  xSecurityID: Integer;
begin
  xStr := TStringList.Create;
  try
    xStr.LoadFromFile(AFileName,TEncoding.UTF8);
    iCount := xStr.Count;
    if iCount > 1 then
    begin
      if GetIsSecurity(ANameCandel) then
        SetDeleteSecurity(ANameCandel);
      xSecurityID := GetInsertSecurity(ANameCandel);
      for i := 1 to iCount - 1 do
      begin
        xS := xStr[i];
        xCandel := TCandel.Cretae(xS);
        SetInsertCandel(xSecurityID,xCandel);
        if Assigned(ACallBakProgerss) then
          ACallBakProgerss(i, iCount);
      end;
    end;
  finally
    FreeAndNil(xStr);
  end;
end;


procedure TMainForm.CallBakProgerss(const AValue, AValueMax: Integer);
begin
  ProgressBar1.Value := AValue;
  ProgressBar1.Max := AValueMax;
  Application.ProcessMessages;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

destructor TMainForm.Destroy;
begin

  inherited;
end;

procedure TMainForm.ImportButtonClick(Sender: TObject);
begin
  OpenDialog.InitialDir := ExtractFilePath(ParamStr(0));
  if OpenDialog.Execute then
  begin
    var xFileName := OpenDialog.FileName;
    var xName := ExtractFileName(xFileName);
    SetImport(xName,xFileName,CallBakProgerss);
  end;
end;

initialization

finalization
  if Assigned(localDataModuleDB) then
    FreeAndNil(localDataModuleDB);

end.
