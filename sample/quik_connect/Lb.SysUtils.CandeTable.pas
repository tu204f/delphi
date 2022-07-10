unit Lb.SysUtils.CandeTable;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  Quik.ValueTable,
  Quik.Manager.DDE;

type
  ///<summary>Источник данных</summary>
  TSourceCandel = class(TQuikTable)
  private
    function GetDate: TDateTime;
    function GetTime: TDateTime;
    function GetClose: Double;
    function GetHigh: Double;
    function GetLow: Double;
    function GetOpen: Double;
  public
    procedure SetLast(const ACount: Integer = 0);
    property Date: TDateTime read GetDate;
    property Time: TDateTime read GetTime;
    property Open: Double read GetOpen;
    property High: Double read GetHigh;
    property Low: Double read GetLow;
    property Close: Double read GetClose;
  end;

  ///<summary>Источник данных, на индикаторе средння скользащая</summary>
  TSourceCandelMA = class(TQuikTable)
  private
  public
  end;

implementation

{ TSourceCandel }

procedure TSourceCandel.SetLast(const ACount: Integer);
begin
  if Self.Count > 0 then
    Self.RowID := (Self.Count - 1) - ACount
  else
    Self.Last;
end;

function TSourceCandel.GetDate: TDateTime;
begin
  Result := Self.ByName['DATE'].AsDate;
end;

function TSourceCandel.GetTime: TDateTime;
begin
  Result := Self.ByName['TIME'].AsTime;
end;

function TSourceCandel.GetOpen: Double;
begin
  Result := Self.ByName['OPEN'].AsDouble;
end;

function TSourceCandel.GetHigh: Double;
begin
  Result := Self.ByName['HIGH'].AsDouble;
end;

function TSourceCandel.GetLow: Double;
begin
  Result := Self.ByName['LOW'].AsDouble;
end;

function TSourceCandel.GetClose: Double;
begin
  Result := Self.ByName['CLOSE'].AsDouble;
end;

end.
