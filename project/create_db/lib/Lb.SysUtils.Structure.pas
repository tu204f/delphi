unit Lb.SysUtils.Structure;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections;

type
  TStructure = class(TObject)
  private
    FFileName: String;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    property FileName: String read FFileName write FFileName;
  end;

implementation

uses
  Lb.DataModuleDB;

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
      GetDB.DefaultConnection(FFileName);
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
