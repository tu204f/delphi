unit Lb.ObjectAndValue;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Variants;

type
  TParamObject = class;

  TCustimParam = class(TObject)
  public type
    TTypeParam = (tpObject,tpValue);
  private
    FName: String;
    FTypeParam: TTypeParam;
    FParent: TParamObject;
  protected
    property TypeParam: TTypeParam read FTypeParam write FTypeParam;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Parent: TParamObject read FParent write FParent;
    property Name: String read FName write FName;
  end;

  ///<summary>Объект, который хранит данные или значение</summary>
  TParamValue = class(TCustimParam)
  public type
    TTypeValue = (tvString,tvNumber,tvDate,tvTime,tvDateTime);
  private
    FValue: String;
    FTypeValue: TTypeValue;
    function GetAsString: String;
    function GetAsInteger: Integer;
    function GetAsDouble: Double;
    function GetAsDateTime: TDateTime;
    function GetAsDate: TDate;
    function GetAsTime: TTime;
    procedure SetAsString(const AValue: String);
    procedure SetAsInteger(const AValue: Integer);
    procedure SetAsDouble(const AValue: Double);
    procedure SetAsDateTime(const AValue: TDateTime);
    procedure SetAsDate(const AValue: TDate);
    procedure SetAsTime(const AValue: TTime);
  protected
    property TypeValue: TTypeValue read FTypeValue write FTypeValue;
    property Value: String read FValue write FValue;
  public
    constructor Create; override;
    destructor Destroy; override;
    property AsString: String read GetAsString write SetAsString;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsDate: TDate read GetAsDate write SetAsDate;
    property AsTime: TTime read GetAsTime write SetAsTime;
  end;
  TParamValueList = TObjectList<TParamValue>;

  TParamObjectList = TObjectList<TParamObject>;

  ///<summary>Параметор - объект/summary>
  TParamObject = class(TCustimParam)
  (****************************************************************************)
  {параметры объекта}
  private
    FParamValues: TParamValueList;
  protected
    procedure ClearParamValues;
    function GetIndexOfParamValue(const AName: String): Integer;
    function GetCreateParamValue(const AName: String): TParamValue;
    property ParamValues: TParamValueList read FParamValues;
  (****************************************************************************)
  {принадлежность объекту}
  private
    FParamObjects: TParamObjectList;
    function GetItemObjects(Name: String): TParamObject;
    function GetItemValues(Name: String): TParamValue;
    function GetCountObjects: Integer;
    function GetCountValues: Integer;
  protected
    procedure ClearParamObjects;
    function GetIndexOfParamObject(const AName: String): Integer;
    function GetCreateParamObject(const AName: String): TParamObject;
    property ParamObjects: TParamObjectList read FParamObjects;
  (****************************************************************************)
  public
    constructor Create; override;
    destructor Destroy; override;
    property ItemValues[Name: String]: TParamValue read GetItemValues;
    property ItemObjects[Name: String]: TParamObject read GetItemObjects;

    property CountValues: Integer read GetCountValues;
    property CountObjects: Integer read GetCountObjects;
  end;

procedure SetSaveToParamObject(const AParamObject: TParamObject; const AFileName: String = 'tmp_maps.json');
procedure SetLoadToParamObject(const AParamObject: TParamObject; const AFileName: String = 'tmp_maps.json');

implementation

const
  PARAM_CHAR = 'АБВГДУЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯабвгдуёжзийклмнопрстуфхцчшщъыьэюя''';


function GeToString(const AIndex: Integer): String;
var
  xS: String;
begin
  xS := AIndex.ToString;
  case xS.Length of
    1: Result := '000' + xS;
    2: Result := '00' + xS;
    3: Result := '0' + xS;
    4: Result := xS;
  else
    Result := xS;
  end;
end;

function GetControlChar(const C: Char): String;
var
  xS: String;
//  xIndex: Integer;
begin
  xS := '';
  case C of
    '"': xS := '\"';
    '\': xS := '\\';
    '/': xS := '\/';
    #9 : xS := '\t';
    #10: xS := '\n';
    #11: xS := '\v';
    #12: xS := '\f';
    #13: xS := '\r';
  else
//    xIndex := Pos(C,PARAM_CHAR);
//    if xIndex > 0 then
//      xS := '\u' + GeToString(xIndex)
//    else
      xS := C;
  end;
  Result := xS;
end;

function GetControlCharToStr(const S: String): String;
var
  tmpS: String;
  xC: Char;
begin
  tmpS := '';
  for xC in S do
    tmpS := tmpS + GetControlChar(xC);
  Result := tmpS;
end;

function GetStrToFloat(const AValue: String): Double;
var
  xOldDecimalSeparator: Char;
begin
  xOldDecimalSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';
  Result := StrToFloatDef(AValue,0);
  FormatSettings.DecimalSeparator := xOldDecimalSeparator;
end;

function GetFloatToStr(const AValue: Double): String;
var
  xOldDecimalSeparator: Char;
begin
  xOldDecimalSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';
  Result := FloatToStr(AValue);
  FormatSettings.DecimalSeparator := xOldDecimalSeparator;
end;

procedure SetSaveToParamObject(const AParamObject: TParamObject; const AFileName: String);
var
  xStr: TStrings;

  function GetSpace(const ACount: Integer): String;
  const
    TAB_SPACE = '   ';
  var
    i: Integer;
    xS: String;
  begin
    xS := '';
    if ACount > 0 then
      for i := 0 to ACount - 1 do
        xS := xS + TAB_SPACE;
    Result := xS;
  end;

  procedure SetParamObejct(ACountSpace: Integer; AParamObject: TParamObject; ACtiveComma: Boolean);
  var
    xS: String;
    i, iCount: Integer;
    xParamValue: TParamValue;
    xParamObject: TParamObject;
  begin
    xStr.Add(GetSpace(ACountSpace) + '"' + AParamObject.Name + '":{');
    // ----------------------------------------------------------------------
    // Значение объектов
    iCount := AParamObject.CountValues;
    if iCount > 0 then
    begin
      for i := 0 to iCount - 1 do
      begin
        xParamValue := AParamObject.ParamValues[i];
        case xParamValue.TypeValue of
          tvString,tvDate,tvTime,tvDateTime: xS := GetSpace(ACountSpace + 1) + '"' + xParamValue.Name + '":"' + GetControlCharToStr(xParamValue.Value) + '"';
          tvNumber: xS := GetSpace(ACountSpace + 1) + '"' + xParamValue.Name + '":' + xParamValue.Value;
        end;
        if AParamObject.CountObjects > 0 then
          xS := xS + ','
        else
        begin
          if i < (iCount - 1) then
            xS := xS + ',';
        end;
        xStr.Add(xS);
      end;
    end;
    // ------------------------------------------------------------------------
    iCount := AParamObject.CountObjects;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xParamObject := AParamObject.ParamObjects[i];
        SetParamObejct(ACountSpace + 1,xParamObject,(i < (iCount - 1)));
      end;
    // ------------------------------------------------------------------------
    if ACtiveComma then
      xStr.Add(GetSpace(ACountSpace) + '},')
    else
      xStr.Add(GetSpace(ACountSpace) + '}')
  end;

var
  xFileName: String;
var
  i, iCount: Integer;
  xParamObject: TParamObject;
begin
  xStr := TStringList.Create;
  try
    xStr.Add('{');
    iCount := AParamObject.CountObjects;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xParamObject := AParamObject.ParamObjects[i];
        SetParamObejct(1,xParamObject,(i < (iCount - 1)));
      end;
    xStr.Add('}');

    xFileName := ExtractFilePath(ParamStr(0))  + AFileName;
    xStr.SaveToFile(xFileName);
  finally
    FreeAndNil(xStr);
  end;
end;

procedure SetLoadToParamObject(const AParamObject: TParamObject; const AFileName: String);

  // Загружать график
  function GetNameToObject(const S: String): String;
  var
    xS: String;
    xIndBegin, xIndEnd: Integer;
  begin
    // xIndEnd := Pos('_$',S);
    // if xIndEnd <= 0 then
    // begin
    xIndBegin := Pos('"',S);
    xIndEnd := Pos('"',S,xIndBegin + 1);
    // end
    // else
    //   xIndBegin := Pos('"',S);
    xS := Copy(S,xIndBegin + 1,(xIndEnd - xIndBegin) - 1);
    Result := xS;
  end;

var
  xS: String;
  xStr: TStrings;
  xFileName: String;
var
  xNameObject: String;
begin
  AParamObject.ClearParamValues;
  AParamObject.ClearParamObjects;
  xStr := TStringList.Create;
  try
    xFileName := ExtractFilePath(ParamStr(0))  + AFileName;
    if FileExists(xFileName) then
    begin
      xStr.LoadFromFile(xFileName);
      for xS in xStr do
      begin
        if Pos('":{',xS) > 0 then
        begin
          // Создание объета
          xNameObject := GetNameToObject(xS);
        end
        else if Pos('":',xS) > 0 then
        begin
          // Значение работы

        end
        else if Pos('}',xS) > 0 then
        begin
          // Завершение объекта

        end;
      end;
    end;
  finally
    FreeAndNil(xStr);
  end;
end;

{ TCustimParam }

constructor TCustimParam.Create;
begin
  FName := '';
  FParent := nil;
end;

destructor TCustimParam.Destroy;
begin

  inherited;
end;


{ TParamValue }

constructor TParamValue.Create;
begin
  inherited Create;
  FValue := '';
  FTypeParam := TParamValue.TTypeParam.tpValue;
end;

destructor TParamValue.Destroy;
begin

  inherited;
end;

function TParamValue.GetAsString: String;
begin
  Result := FValue;
end;

function TParamValue.GetAsInteger: Integer;
begin
  Result := StrToIntDef(FValue,0);
end;

function TParamValue.GetAsDouble: Double;
begin
  Result := GetStrToFloat(FValue);
end;

function TParamValue.GetAsDateTime: TDateTime;
begin
  Result := StrToDateTimeDef(FValue,0);
end;

function TParamValue.GetAsDate: TDate;
begin
  Result := StrToDateDef(FValue,0);
end;

function TParamValue.GetAsTime: TTime;
begin
  Result := StrToTime(FValue)
end;

procedure TParamValue.SetAsString(const AValue: String);
begin
  FValue := AValue;
  FTypeValue := tvString;
end;

procedure TParamValue.SetAsInteger(const AValue: Integer);
begin
  FValue := IntToStr(AValue);
  FTypeValue := tvNumber;
end;

procedure TParamValue.SetAsDouble(const AValue: Double);
begin
  FValue := GetFloatToStr(AValue);
  FTypeValue := tvNumber;
end;

procedure TParamValue.SetAsDateTime(const AValue: TDateTime);
begin
  FValue := DateTimeToStr(AValue);
  FTypeValue := tvDateTime
end;

procedure TParamValue.SetAsDate(const AValue: TDate);
begin
  FValue := DateToStr(AValue);
  FTypeValue := tvDate;
end;

procedure TParamValue.SetAsTime(const AValue: TTime);
begin
  FValue := TimeToStr(AValue);
  FTypeValue := tvTime;
end;

{ TParamObject }

constructor TParamObject.Create;
begin
  inherited Create;
  FTypeParam := TParamValue.TTypeParam.tpObject;
  FParamValues := TParamValueList.Create;
  FParamObjects := TParamObjectList.Create;
  Self.ClearParamValues;
end;

destructor TParamObject.Destroy;
begin
  FreeAndNil(FParamValues);
  FreeAndNil(FParamObjects);
  inherited;
end;

procedure TParamObject.ClearParamValues;
begin
  FParamValues.Clear;
end;

function TParamObject.GetCreateParamValue(const AName: String): TParamValue;
var
  xIndex: Integer;
  xParamValue: TParamValue;
begin
  xIndex := GetIndexOfParamValue(AName);
  if xIndex < 0 then
  begin
    xParamValue := TParamValue.Create;
    xParamValue.Name := AName;
    xParamValue.Parent := Self;
    FParamValues.Add(xParamValue);
  end
  else
    xParamValue := FParamValues[xIndex];
  Result := xParamValue;
end;

function TParamObject.GetIndexOfParamValue(const AName: String): Integer;
var
  i, Count: Integer;
  xParamValue: TParamValue;
begin
  Result := -1;
  Count := FParamValues.Count;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      xParamValue := FParamValues[i];
      if SameText(xParamValue.Name,AName) then
      begin
        Result := i;
        Break;
      end;
    end;
end;

procedure TParamObject.ClearParamObjects;
begin
  FParamObjects.Clear;
end;

function TParamObject.GetIndexOfParamObject(const AName: String): Integer;
var
  i, Count: Integer;
  xParamObject: TParamObject;
begin
  Result := -1;
  Count := FParamObjects.Count;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      xParamObject := FParamObjects[i];
      if SameText(AName,xParamObject.Name) then
      begin
        Result := i;
        Break;
      end;
    end;
end;

function TParamObject.GetCreateParamObject(const AName: String): TParamObject;
var
  xIndex: Integer;
  xParamObject: TParamObject;
begin
  xIndex := GetIndexOfParamObject(AName);
  if xIndex < 0 then
  begin
    xParamObject := TParamObject.Create;
    xParamObject.Name := AName;
    xParamObject.Parent := Self;
    FParamObjects.Add(xParamObject);
  end
  else
    xParamObject := FParamObjects[xIndex];
  Result := xParamObject;
end;

function TParamObject.GetItemObjects(Name: String): TParamObject;
begin
  Result := GetCreateParamObject(Name);
end;

function TParamObject.GetItemValues(Name: String): TParamValue;
begin
  Result := GetCreateParamValue(Name);
end;

function TParamObject.GetCountObjects: Integer;
begin
  Result := FParamObjects.Count;
end;

function TParamObject.GetCountValues: Integer;
begin
  Result := FParamValues.Count;
end;

end.
