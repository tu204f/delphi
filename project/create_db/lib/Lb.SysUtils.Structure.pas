unit Lb.SysUtils.Structure;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  Lb.Create.DB;

type
  TStructure = class(TObject)
  private
    FFileName: String;
  protected
    procedure SetCreateDataBase;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    property FileName: String read FFileName write FFileName;
  end;

implementation

uses
  Lb.DataModuleDB,
  Lb.Resource;

var
  localDataBase: TDataModuleDB = nil;

function GetDB: TDataModuleDB;
begin
  if not Assigned(localDataBase) then
    localDataBase := TDataModuleDB.Create(nil);
  Result := localDataBase;
end;

procedure SetFinalization;
begin
  if Assigned(localDataBase) then
    FreeAndNil(localDataBase);
  localDataBase := nil;
end;

function GetIsConnect: Boolean;
begin
  Result := False;
  if Assigned(localDataBase) then
    Result := localDataBase.Connected;
end;

{ TStructure }

constructor TStructure.Create;
begin
  FFileName := '';
end;

destructor TStructure.Destroy;
begin
  SetFinalization;
  inherited;
end;

procedure TStructure.Open;
begin
  if not GetIsConnect then
    if not FFileName.IsEmpty then
    begin
      GetDB.DefaultConnection(FFileName);
      SetCreateDataBase;
    end;
end;

procedure TStructure.SetCreateDataBase;
var
  xScript: TStrings;
begin
  xScript :=  TStringList.Create;
  try
    SetResourceParams('data_base',xScript);
    GetDB.GetExecSQL(xScript.Text);
  finally
    FreeAndNil(xScript);
  end;
end;

procedure TStructure.Close;
begin
  if GetIsConnect then
    SetFinalization;
end;

initialization

finalization
  SetFinalization;

end.
