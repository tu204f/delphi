unit Lb.Source.Json;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections;

type
  ///<summary>
  /// Значение работы
  ///</summary>
  TJsValue = class(TObject)
    Name: String;
    Value: String;
  private
    function GetText: String;
  public
    constructor Create; overload;
    constructor Create(const AName, AValue: String); overload;
    property Text: String read GetText;
  end;
  TJsValueList = TObjectList<TJsValue>;

  ///<summary>
  /// Массив
  ///</summary>
  TJsArray = class(TJsValue)
  private
    FValues: TJsValueList;
    function GetText: String;
  public
    constructor Create;
    destructor Destroy; override;
    property Values: TJsValueList read FValues;
    property Text: String read GetText;
  end;

  ///<summary>
  /// Объект работы
  ///</summary>
  TJsObject = class(TJsValue)
  private
    FValues: TJsValueList;
    function GetText: String;
  public
    constructor Create;
    destructor Destroy; override;
    function IndexOfName(const AName: String): Integer;
    procedure SetValue(const AName, AValue: String);
    property Values: TJsValueList read FValues;
    property Text: String read GetText;
  end;

implementation

{ TJsValue }

constructor TJsValue.Create;
begin
  Name := '';
  Value:= '';
end;

constructor TJsValue.Create(const AName, AValue: String);
begin
  Name := AName;
  Value:= AValue;
end;

function TJsValue.GetText: String;
begin
  Result := Format('"%s":"%s"',[Name,Value]);
end;

{ TJsArray }

constructor TJsArray.Create;
begin
  inherited;
  FValues := TJsValueList.Create;
end;

destructor TJsArray.Destroy;
begin
  FreeAndNil(FValues);
  inherited;
end;

function TJsArray.GetText: String;
var
  xS: String;
  xValue: TJsValue;
begin
  xS := '[';
  if FValues.Count > 0 then
  begin
    for var i := 0 to FValues.Count - 1 do
    begin
      xValue := FValues.Items[i];
      xS := xS + xValue.Text;
      if (FValues.Count - 1) = 0 then
        xS := xS + xValue.Text + ']'
      else
        xS := xS + xValue.Text + ','
    end;
  end;
  Result := xS;
end;

{ TJsObject }

constructor TJsObject.Create;
begin
  FValues := TJsValueList.Create;
end;

destructor TJsObject.Destroy;
begin
  FreeAndNil(FValues);
  inherited;
end;

function TJsObject.GetText: String;
var
  xS: String;
  xValue: TJsValue;
begin
  if Name.IsEmpty then
    xS := '{'
  else
    xS := '"' + Name + '":{';
  if FValues.Count > 0 then
  begin
    for var i := 0 to FValues.Count - 1 do
    begin
      xValue := FValues.Items[i];
      xS := xS + xValue.Text;
      if (FValues.Count - 1) = 0 then
        xS := xS + xValue.Text + '}'
      else
        xS := xS + xValue.Text + ','
    end;
  end;
  Result := xS;
end;

function TJsObject.IndexOfName(const AName: String): Integer;
var
  xValue: TJsValue;
  i, iCount: Integer;
begin
  Result := -1;
  iCount := FValues.Count;
  if iCount > 0 then
  begin
    for i := 0 to iCount - 1 do
    begin
      xValue := FValues[i];
      if SameText(xValue.Name,AName) then
      begin
        Result := i;
        Break;
      end;
    end;
  end;
end;

procedure TJsObject.SetValue(const AName, AValue: String);
var
  xValue: TJsValue;
  xIndex: Integer;
begin
  xIndex := IndexOfName(AName);
  if (xIndex >= 0) and (xIndex < FValues.Count) then
  begin
    xValue := FValues[xIndex];
    xValue.Value := AValue;
  end
  else
  begin
    xValue := TJsValue.Create(AName,AValue);
    FValues.Add(xValue);
  end;
end;

end.
