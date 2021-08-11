(*******************************************************************************
  Параметрическое сохрание данных - в локальной базы данных SQLile3
  в качестве документа DocDB
*******************************************************************************)
unit Lb.Setting.Values.SQLiteDB;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Variants,
  Lb.Setting.Values;

type
  TConfigSQLite3 = class(TConfig)
  private
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Open; override;
    procedure Close; override;
  end;

implementation

{ TConfigSQLite3 }

constructor TConfigSQLite3.Create;
begin
  inherited;

end;

destructor TConfigSQLite3.Destroy;
begin

  inherited;
end;

procedure TConfigSQLite3.Open;
begin

end;

procedure TConfigSQLite3.Close;
begin

end;

end.
