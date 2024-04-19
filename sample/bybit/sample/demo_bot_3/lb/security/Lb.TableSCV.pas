unit Lb.TableSCV;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections;

type
  ///<summary>
  /// Таблица в формате CSV
  ///</summary>
  TTableSCV = class(TObject)
  public type
    ///<summary>
    /// Поле таблицы
    ///</summary>
    TField = class(TObject)
    private
      FName: String;
      FSources: TStrings;
      function GetItems(Index: Integer): String;
      function GetCount: Integer;
    protected
      procedure Insert(const AValue: String);
    public
      constructor Create; virtual;
      destructor Destroy; override;
      procedure Clear;
      property Items[Index: Integer]: String read GetItems;
      property Name: String read FName write FName;
      property Count: Integer read GetCount;
    end;

    ///<summary>
    /// Список полей
    ///</summary>
    TFieldList = class(TObjectList<TField>)
    public
      function IndexOf(const AName: String): Integer;
    end;

  private
    FRow: Integer;
    FFields: TFieldList;
    function GetColCount: Integer;
    function GetRowCount: Integer;
    function GetCells(ACol, ARow: Integer): String;
  protected
    procedure SetParser(const ASources: TStrings);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Load(const AFileName: String);
  public
    property Fields: TFieldList read FFields;
    property Cells[ACol, ARow: Integer]: String read GetCells;
    property ColCount: Integer read GetColCount;
    property RowCount: Integer read GetRowCount;
  public
    procedure First;
    procedure Next;
    function EOF: Boolean;
    function FieldByName(const AName: String): String;
  end;

implementation

{ TTableSCV.TField }

constructor TTableSCV.TField.Create;
begin
  FSources := TStringList.Create;
end;

destructor TTableSCV.TField.Destroy;
begin
  FreeAndNil(FSources);
  inherited;
end;

procedure TTableSCV.TField.Clear;
begin
  FSources.Clear;
end;

function TTableSCV.TField.GetItems(Index: Integer): String;
begin
  Result := '';
  if (Index >= 0) and (Index < FSources.Count) then
    Result := FSources[Index];
end;

procedure TTableSCV.TField.Insert(const AValue: String);
begin
  FSources.Add(AValue);
end;

function TTableSCV.TField.GetCount: Integer;
begin
  Result := FSources.Count;
end;

{ TTableSCV.TFieldList }

function TTableSCV.TFieldList.IndexOf(const AName: String): Integer;
var
  i: Integer;
  xField: TTableSCV.TField;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    xField := Items[i];
    if SameText(xField.Name,AName) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

{ TTableSCV }

constructor TTableSCV.Create;
begin
  FFields := TFieldList.Create;
end;

destructor TTableSCV.Destroy;
begin
  FreeAndNil(FFields);
  inherited;
end;

procedure TTableSCV.SetParser(const ASources: TStrings);

  procedure _ParserStr(const S: String; AStrings: TStrings);
//  var
//    tmpS: String;
  begin
//    tmpS := '';
//    AStrings.Clear;
//    for var xC in S do
//    begin
//      if xC = ';' then
//      begin
//        AStrings.Add(tmpS);
//        tmpS := '';
//      end
//      else
//        tmpS := tmpS + xC;
//    end;
//    AStrings.Add(tmpS);
    AStrings.Delimiter := ';';
    AStrings.DelimitedText := S;
  end;

  procedure _ParserFields(const S: String);
  var
    xField: TTableSCV.TField;
    xNames: TStrings;
  begin
    xNames := TStringList.Create;
    try
      _ParserStr(S,xNames);
      for var xName in xNames do
      begin
        xField := TTableSCV.TField.Create;
        xField.Name := xName;
        FFields.Add(xField);
      end;
    finally
      FreeAndNil(xNames);
    end;
  end;

  procedure _ParserValues(const S: String);
  var
    xField: TTableSCV.TField;
    xValues: TStrings;
  begin
    xValues := TStringList.Create;
    try
      _ParserStr(S,xValues);

      if not (xValues.Count = FFields.Count) then
      begin
        var xMsg := 'Количество полей не соответствует количеству значений';
        raise Exception.Create('Error Message: ' + xMsg);
      end;

      for var i := 0 to xValues.Count - 1 do
      begin
        xField := FFields[i];
        xField.Insert(xValues[i]);
      end;
    finally
      FreeAndNil(xValues);
    end;
  end;

begin
  if ASources.Count > 0 then
    for var i := 0 to ASources.Count - 1 do
    begin
      var xS := ASources[i];
      if i = 0 then
        _ParserFields(xS)
      else
        _ParserValues(xS);
    end;
end;

function TTableSCV.GetColCount: Integer;
begin
  Result := FFields.Count;
end;

function TTableSCV.GetRowCount: Integer;
begin
  Result := 0;
  if ColCount > 0 then
    Result := FFields[0].Count;
end;

function TTableSCV.GetCells(ACol, ARow: Integer): String;
var
  xField: TField;
begin
  Result := '';
  if (ACol >= 0) and (ACol < FFields.Count) then
  begin
    xField := FFields[ACol];
    Result := xField.Items[ARow];
  end;
end;

procedure TTableSCV.First;
begin
  FRow := 0;
end;

procedure TTableSCV.Next;
begin
  Inc(FRow);
end;

function TTableSCV.EOF: Boolean;
begin
  Result := FRow = (RowCount - 1);
end;

function TTableSCV.FieldByName(const AName: String): String;
var
  xIndex: Integer;
  xField: TField;
begin
  Result := '';
  xIndex := FFields.IndexOf(AName);
  if xIndex >= 0 then
  begin
    xField := FFields[xIndex];
    Result := xField.Items[FRow];
  end;
end;

procedure TTableSCV.Load(const AFileName: String);
var
  xStr: TStrings;
  xFileName: String;
begin
  xFileName := ExtractFilePath(ParamStr(0)) + AFileName;
  if FileExists(xFileName) then
  begin
    xStr := TStringList.Create;
    try
      xStr.LoadFromFile(xFileName);
      SetParser(xStr);
    finally
      FreeAndNil(xStr);
    end;
  end;
end;

end.
