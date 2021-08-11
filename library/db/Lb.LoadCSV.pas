unit Lb.LoadCSV;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections;

type
  TMenuRow = record
    BeginData: TDateTime;
    EndTime: TDateTime;
    Flag: Integer;
    NameMenu: String;
    Category: String;
    Gruppa: String;
    Weight: Double;
    Price: Double;
    Descripot: String;
  private
    procedure SetValue(const AValue: String);
  public
    property Value: String write SetValue;
  end;

  TMenuRowList = TList<TMenuRow>;

  /// <summary>
  /// Список значения работы
  /// </summary>
  TMenuRowCSV = class(TObject)
  private
    FFileName: String;
    FRows: TMenuRowList;
    FCurrentDate: TDateTime;
    procedure SetFileName(const Value: String);
  protected
    FSelected: TMenuRowList;
    FGrupMenu: TMenuRowList;
    procedure SetCurrentDate(const Value: TDateTime);
    procedure SetLoadStrings(const AStrings: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
    property FileName: String read FFileName write SetFileName;
    property Rows: TMenuRowList read FRows;
    property CurrentDate: TDateTime read FCurrentDate write SetCurrentDate;
    /// <summary>
    /// выбранный сисок по категориям
    /// </summary>
    property Selected: TMenuRowList read FSelected;

  public
    procedure SetGrupMenu(ACurrentDate: TDateTime; ACategory, AGrupd: String);
    property GrupMenu: TMenuRowList read FGrupMenu;
  end;

implementation

{ TMenuRow }

procedure TMenuRow.SetValue(const AValue: String);

  function GetStrValue(AStrings: TStrings; AIndex: Integer): String;
  begin
    if (AIndex >= 0) and (AIndex < AStrings.Count) then
      Result := AStrings[AIndex]
    else
      Result := '';
  end;

  procedure SetField(const AStrings: TStrings);
  begin
    if Assigned(AStrings) then
    begin
      Self.BeginData := StrToDateDef(GetStrValue(AStrings, 0), 0);
      Self.EndTime := StrToDateDef(GetStrValue(AStrings, 1), 0);
      Self.Flag := 0;
      Self.NameMenu := GetStrValue(AStrings, 3);
      Self.Category := GetStrValue(AStrings, 7);
      Self.Gruppa := GetStrValue(AStrings, 8);
      Self.Weight := StrToFloatDef(GetStrValue(AStrings, 9), 0);
      Self.Price := StrToFloatDef(GetStrValue(AStrings, 10), 0);
      Self.Descripot := GetStrValue(AStrings, 11);
    end;
  end;

var
  xB2: Integer;
  C: Char;
  xB: Boolean;
  tmpS: String;
  xStr: TStringList;
begin
  xStr := TStringList.Create;
  try
    xB2 := 0;
    tmpS := '';
    xB := False;
    for C in AValue do
    begin
      // ----------------
      if C = '"' then
      begin
        if xB then
          xStr.Add(tmpS);
        xB := not xB;
        tmpS := '';
        Continue;
        Inc(xB2);
      end;
      // ----------------
      if xB then
      begin
        tmpS := tmpS + C;
        Continue;
      end;
      // ----------------
      if C = ';' then
      begin
        xStr.Add(tmpS);
        tmpS := '';
      end
      else
        tmpS := tmpS + C;
      // ----------------
    end;

    if xB2 > 0 then
      xStr.Add(tmpS);

    SetField(xStr);
  finally
    FreeAndNil(xStr);
  end;
end;

{ TMenuRowCSV }

constructor TMenuRowCSV.Create;
begin
  FRows := TMenuRowList.Create;
  FSelected := TMenuRowList.Create;
  FGrupMenu := TMenuRowList.Create;
end;

destructor TMenuRowCSV.Destroy;
begin
  FreeAndNil(FGrupMenu);
  FreeAndNil(FSelected);
  FreeAndNil(FRows);
  inherited;
end;

procedure TMenuRowCSV.SetFileName(const Value: String);
var
  xStr: TStringList;
begin
  FFileName := Value;
  if FileExists(FFileName) then
  begin
    xStr := TStringList.Create;
    try
      xStr.LoadFromFile(FFileName);
      SetLoadStrings(xStr);
    finally
      FreeAndNil(xStr);
    end;
  end;
end;

procedure TMenuRowCSV.SetLoadStrings(const AStrings: TStrings);
var
  xS: String;
  xRow: TMenuRow;
  i, Count: Integer;
begin
  if Assigned(AStrings) then
  begin
    FRows.Clear;
    Count := AStrings.Count;
    if Count > 0 then
      for i := 0 to Count - 1 do
      begin
        xS := AStrings[i];
        xRow.Value := xS;
        FRows.Add(xRow);
      end;
  end;
end;

procedure TMenuRowCSV.SetCurrentDate(const Value: TDateTime);
var
  xRow: TMenuRow;
  i, Count: Integer;
begin
  FCurrentDate := Value;
  Count := FRows.Count;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      xRow := FRows[i];
      if (xRow.BeginData <= FCurrentDate) and (FCurrentDate <= xRow.EndTime)
      then
      begin
        if xRow.Gruppa.IsEmpty then
          xRow.Gruppa := xRow.Category;
        FSelected.Add(xRow);
      end;
    end;
end;

procedure TMenuRowCSV.SetGrupMenu(ACurrentDate: TDateTime;
  ACategory, AGrupd: String);
var
  xRow: TMenuRow;
  i, Count: Integer;
begin
  FGrupMenu.Clear;
  Count := FRows.Count;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      xRow := FRows[i];
      if (xRow.BeginData <= FCurrentDate) and (FCurrentDate <= xRow.EndTime) and
        SameStr(ACategory, xRow.Category) {and SameStr(AGrupd, xRow.Gruppa)} then
        FGrupMenu.Add(xRow);
    end;
end;

end.
