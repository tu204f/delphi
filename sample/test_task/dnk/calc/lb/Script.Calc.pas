unit Script.Calc;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Contnrs,
  System.Classes,
  System.Generics.Collections,
  System.RegularExpressions;

type
  TScript = class;
  TSetVar = class;

  TExpression = class;

  TTypeToken = (
    ttNull,     // Пустоное занчение
    ttVar,      // Переменная
    ttNumber,   // Число
    ttOperator  // Оператор
  );

  TToken = record
    Value: String;
    Priority: Integer;
    TypeToken: TTypeToken;
  public
    function ToString: String;
    procedure Clear;
  end;

  ///<summary>
  /// Представляем бинарный узел AST
  ///</summary>
  TExpression = class(TObject)
  private
    FToken: TToken;
    FLeft: TExpression;
    FRigth: TExpression;
    function GetPriority: Integer;
    function GetName: String;
  public
    constructor Create(AToken: TToken);
    destructor Destroy; override;
    function Eval: Double;
    property Name: String read GetName;
    property Left: TExpression read FLeft write FLeft;
    property Rigth: TExpression read FRigth write FRigth;
    property Token: TToken read FToken write FToken;
    property Priority: Integer read GetPriority;
  end;

  ///<summary>
  /// Парсер — позволяет разложить мат. выражение на токены,
  /// без создание массива токенов.
  // </summary>
  TParser = class(TObject)
  private
    FValue: String;
    FLength: Integer;
    FIndex: Integer;
    procedure SetValue(const Value: String);
  protected
    function ParserToken: TToken;
  public
    constructor Create;
    destructor Destroy; override;
    function First: TToken;
    function Next: TToken;
    function EOF: Boolean;
    property Value: String write SetValue;
  end;

  ///<summary>Калькулятор</summary>
  ///<remarks>Парсинг и создание AST, в один проход</remarks>
  TCalc = class(TObject)
  private
    FScript: TScript;
    FParser: TParser;
    FRoot: TExpression;
    procedure SetValue(const Value: String);
  protected
    function ExpressionOperator(const AParser: TParser): TExpression;
    function ExpressionParser(const AExpression: TExpression; const AParser: TParser): TExpression;
  public
    constructor Create;
    destructor Destroy; override;
    ///<summary>Создается AST</summary>
    procedure Compile;
    function ReturnValue: Double;
    property Value: String write SetValue;
    property Parser: TParser read FParser;
    property Root: TExpression read FRoot;
    property Script: TScript read FScript write FScript;
  end;

  TSetVar = class
  private
    FName: String;
    FCalc: TCalc;
    FScript: TScript;
    procedure SetText(const Value: String);
  public
    constructor Create(AScript: TScript);
    destructor Destroy; override;
    property Text: String write SetText;
    property Calc: TCalc read FCalc;
    property Name: String read FName;
    procedure Execute;
  end;

  TScript = class(TStringList)
  private
    FVars: TStringList;
  public
    constructor Create; overload;
    destructor Destroy; override;
    procedure Compile;
    function Execute: Variant;
  end;

implementation

{ TToken }

procedure TToken.Clear;
begin
  TypeToken := ttNull;
end;

function TToken.ToString: String;
var
  xS: String;
begin
  xS := '"[p:' + Priority.ToString + ']"';
  case TypeToken of
    ttVar: xS := xS + ' V:';
    ttNumber: xS := xS + ' N:';
    ttOperator: xS := xS + ' O:';
  end;

  if Value = '(' then
    xS := xS + ' Token = [()]'
  else
    xS := xS + ' Token = [' + Value + ']';
  Result := xS;
end;

{ TExpression }

constructor TExpression.Create(AToken: TToken);
begin
  FToken := AToken;
  FLeft := nil;
  FRigth := nil;
end;

destructor TExpression.Destroy;
begin
  FreeAndNil(FLeft);
  FreeAndNil(FRigth);
  inherited;
end;

function TExpression.Eval: Double;

  function _StrTokenToFloat(AToken: TToken): Double;
  var
    FS: TFormatSettings;
  begin
    try
      // Вдруг придется в потоке работать
      FS := FormatSettings;
      FS.DecimalSeparator := '.';
      Result := StrToFloat(AToken.Value,FS);
    except
      raise Exception.Create(
        'Error Message: Неверный формат числа' + sLineBreak +
        'example: 1256.654' + sLineBreak +
        'token: ' + AToken.ToString
      );
    end;
  end;

var
  xValue: Double;
begin
  if Assigned(FLeft) and Assigned(FRigth) then
  begin
    case FToken.Value[1] of
      '+': Result := FLeft.Eval + FRigth.Eval;
      '-': Result := FLeft.Eval - FRigth.Eval;
      '*': Result := FLeft.Eval * FRigth.Eval;
      '/': begin
        xValue := FRigth.Eval;
        if xValue > 0 then
          Result := FLeft.Eval / FRigth.Eval
        else
          raise Exception.Create(
            'Error Message: Division by zero' + sLineBreak +
            'left: ' + FLeft.ToString + sLineBreak +
            'rigth: ' + FRigth.ToString
          );
      end;
    else
      raise Exception.Create('Error Message: Unknown operation');
    end;
  end
  else if Assigned(FLeft) then
  begin
    // Левая операция это всегда скобка
    Result := FLeft.Eval;
  end
  else if Assigned(FRigth) then
  begin
    case FToken.Value[1] of
      '+': Result := FRigth.Eval;
      '-': Result := -1 * FRigth.Eval;
    else
      raise Exception.Create(
        'Error Message: Unary operation' + sLineBreak +
        'example: +1 or -1' + sLineBreak +
        'token: ' + FToken.ToString
      );
    end;
  end
  else
  begin
    if FToken.TypeToken in [ttVar,ttNumber] then
      Result := _StrTokenToFloat(FToken)
    else
      Result := FLeft.Eval;
  end;
end;

function TExpression.GetName: String;
begin
  if FToken.Value = '(' then
    Result := '()'
  else
    Result := FToken.Value;
end;

function TExpression.GetPriority: Integer;
begin
  Result := FToken.Priority;
end;

{ TParser }

constructor TParser.Create;
begin
  FValue := '';
  FIndex := 0;
end;

destructor TParser.Destroy;
begin

  inherited;
end;

function TParser.EOF: Boolean;
begin
  Result := FIndex > FLength;
end;

function TParser.First: TToken;
begin
  FIndex := 1;
  Result := ParserToken;
end;

function TParser.Next: TToken;
begin
  Result := ParserToken;
end;

procedure TParser.SetValue(const Value: String);
begin
  FValue := Value;
  FLength := FValue.Length;
  FIndex := 1;
end;

function TParser.ParserToken: TToken;

  // Самы высокний приоритет 0
  // 0 - Сначало опеределяется чилос
  // 1 - "*" и "/"
  // 2 - "+" и "-"
  // и так далее
  function _GetPriority(const AValue: Char): Integer;
  begin
    case AValue of
      '*','/': Result := 1;
      '+','-': Result := 2;
    else
      Result := 0;
    end;
  end;

var
  xInd: Integer;
  xToken: TToken;
  xChar: Char;
var
  xM: TMatch;
begin
  {todo: генерация токенов - нужно подумать}
  xToken.Value := '';
  xToken.Priority := 0;
  xToken.TypeToken := ttNull;

  if FValue.IsEmpty then
    raise Exception.Create('Error Message: No data');

  if FIndex > FLength then
    raise EAbort.Create('Error Message: Забыл про EOF');

  while (FIndex <= FLength) do
  begin
    xChar := FValue[FIndex];

    // Левые символы
    if CharInSet(xChar,[#0,#10,#13,#32]) then
    begin
      Inc(FIndex);
      if xToken.Value.IsEmpty then
        Continue
      else
        Break;
    end;

    // основном мат. выражение
    if (xToken.TypeToken in [ttNull,ttOperator]) and CharInSet(xChar,['+','-','*','/','(',')']) then
    begin
      xToken.Priority := _GetPriority(xChar);
      xToken.TypeToken := ttOperator;
      Inc(FIndex);
      xToken.Value := xToken.Value + xChar;
      Break;
    end;

    xInd := FIndex;
    if (xInd <= FLength) then
    begin
      {todo: Подумать}
      if (xToken.TypeToken = ttNull) and  CharInSet(FValue[xInd],['A'..'Z','a'..'z']) then
      begin
        xToken.Priority := 0;
        xToken.TypeToken := ttVar;
        Inc(FIndex);
      end
      else
      if (xToken.TypeToken = ttNull) and CharInSet(FValue[xInd],['0'..'9','.']) then
      begin
        xToken.Priority := 0;
        xToken.TypeToken := ttNumber;
        Inc(FIndex);
      end;

      if (xToken.TypeToken = ttVar) and not CharInSet(FValue[xInd],['A'..'Z','a'..'z']) then
      begin
        //Inc(FIndex);
        Break;
      end;

      if (xToken.TypeToken = ttNumber) and not CharInSet(FValue[xInd],['0'..'9','.']) then
      begin
        //Inc(FIndex);
        Break;
      end;

    end;

    xToken.Value := xToken.Value + xChar;
    Inc(FIndex);
  end;
  Result := xToken;
end;

{ TCalc }

constructor TCalc.Create;
begin
  FRoot := nil;
  FParser := TParser.Create;
end;

destructor TCalc.Destroy;
begin
  if Assigned(FParser) then
    FreeAndNil(FParser);
  if Assigned(FRoot) then
    FreeAndNil(FRoot);
  inherited;
end;

function TCalc.ExpressionParser(const AExpression: TExpression; const AParser: TParser): TExpression;

  function _ResultExpression(AExpression: TExpression; AParser: TParser): TExpression;
  begin
    if AParser.EOF then
    begin
      Result := AExpression;
    end
    else
    begin
      Result := ExpressionParser(AExpression,AParser);
    end;
  end;

var
  xExpression: TExpression;
begin
  xExpression := ExpressionOperator(AParser);
  if Assigned(xExpression) then
  begin
    if Assigned(AExpression) then
    begin
      if AExpression.Priority > xExpression.Priority then
      begin
        xExpression.Left := AExpression.Rigth;
        AExpression.Rigth := xExpression;
        xExpression := AExpression;
      end
      else
        xExpression.Left := AExpression;
    end;
    Result := _ResultExpression(xExpression,AParser);
  end
  else
  begin
    Result := _ResultExpression(AExpression,AParser);
  end;
end;

function TCalc.ReturnValue: Double;
begin
  Result := 0;
  try
    if Assigned(FRoot) then
      Result := FRoot.Eval;
  except
    on E: Exception do
      raise Exception.Create('Error Message: Syntax error. ' + E.Message);
  end;
end;

procedure TCalc.SetValue(const Value: String);
begin
  FParser.Value := Value;
end;

function TCalc.ExpressionOperator(const AParser: TParser): TExpression;

  function _CreateExpression(AToken: TToken): TExpression;
  begin
    Result := nil;
    if AToken.TypeToken <> ttNull then
      Result := TExpression.Create(AToken);
  end;

  function _CreateExpressionBracket(AToken: TToken; AParser: TParser): TExpression;
  var
    xExpression: TExpression;
  begin
    Result := nil;
    if AToken.TypeToken <> ttNull then
    begin
      xExpression := _CreateExpression(AToken);
      xExpression.Left := ExpressionOperator(AParser);
      Result := xExpression;
    end;
  end;

var
  xTmp: TToken;
  xStep: Integer;
  xLeft, xOperator, xRigth: TToken;

  xExpression: TExpression;
begin

  xLeft.Clear;
  xOperator.Clear;
  xRigth.Clear;

  {todo: Защита если вдруго не ожиданное действие}
  xStep := 1;
  xTmp := AParser.Next;

  if xTmp.Value = '(' then
  begin
    Result := _CreateExpressionBracket(xTmp, AParser);
    Exit;
  end;

  if xTmp.Value = ')' then
  begin
    Result := nil;
    Exit;
  end;
  case xTmp.TypeToken of
    {todo: Нужно подумать}
    ttVar: begin
      xLeft := xTmp;
      xLeft.Value := FScript.FVars.Values[xLeft.Value];
    end;
    ttNumber: xLeft := xTmp;
    ttOperator: xOperator := xTmp;
  end;

  while not AParser.EOF do
  begin
    xTmp := AParser.Next;
    case xTmp.TypeToken of
      ttNumber: begin
        case xStep of
          1,2: begin
            xRigth := xTmp;
            Break;
          end;
        end;
      end;

      ttVar: begin
        case xStep of
          1,2: begin
            xRigth := xTmp;
            xRigth.Value := FScript.FVars.Values[xRigth.Value];
            Break;
          end;
        end;
      end;

      ttOperator: begin
        if xOperator.TypeToken = ttNull then
          xOperator := xTmp
        else
        begin
          xRigth := xTmp;
          Break;
        end;
      end;

    else
      raise EAbort.Create('Error Message: Повнимательнее так не должно быть');
    end;
    Inc(xStep);
    if xStep > 3 then
      {todo: Вывести список чего ожидали получить}
      raise Exception.Create('Error Message: Unexpected token');
  end;

  {todo: Нужно придумать Check... лень}
  if (xLeft.TypeToken in [ttNull,ttVar,ttNumber]) and
     (xOperator.TypeToken = ttOperator) and
     (xRigth.TypeToken in [ttNumber,ttVar,ttOperator])  then
  begin
    xExpression := _CreateExpression(xOperator);
    xExpression.Left := _CreateExpression(xLeft);
    if xRigth.Value = '(' then
    begin
      xExpression.Rigth := _CreateExpressionBracket(xRigth, AParser);
    end
    else
      xExpression.Rigth := _CreateExpression(xRigth);
  end
  else
    xExpression := _CreateExpression(xLeft);

  Result := xExpression;
end;

procedure TCalc.Compile;
begin
  {todo: }
  try
    FRoot := ExpressionParser(FRoot,FParser);
  except
    // Перехватываем все сообщение об ошибках
    on E: Exception do
      raise Exception.Create('Error Message:' + E.Message);
  end;
end;

{ TScriptLine }

constructor TSetVar.Create(AScript: TScript);
begin
  FScript := AScript;
  FCalc := TCalc.Create;
  FCalc.Script := FScript;
end;

destructor TSetVar.Destroy;
begin
  if Assigned(FCalc) then
    FreeAndNil(FCalc);
  inherited;
end;

procedure TSetVar.SetText(const Value: String);
begin
  {todo: Подумать}
  if Value.IsEmpty then
    Exit;
  var xPosInd := Pos('=',Value);
  if xPosInd > 1 then
  begin
    var xName := Trim(Copy(Value,1,xPosInd - 1));
    var xExpression := Trim(Copy(Value,xPosInd + 1,MaxInt));
    if xName.IsEmpty then
      raise Exception.Create('Не задано имя переменной');
    if xExpression.IsEmpty then
      raise Exception.Create('Ожидается выражение');
    FName := xName;
    FCalc.Value := xExpression;
  end
  else
    raise Exception.Create('Неизвестная инструкция');
end;

procedure TSetVar.Execute;
begin
  FCalc.Compile;
  FScript.FVars.Values[FName] := FCalc.ReturnValue.ToString;
end;

{ TScript }

procedure TScript.Compile;
var
  i, iCount: Integer;
begin
  iCount := Self.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      var xS := Trim(Self.Strings[i]);
      if not xS.IsEmpty then
      begin
        var xSV := TSetVar.Create(Self);
        xSV.Text := xS;
        Self.Objects[i] := xSV;
      end;
    end;
end;

constructor TScript.Create;
begin
  FVars := TStringList.Create;
end;

destructor TScript.Destroy;
begin
  FreeAndNil(FVars);
  inherited;
end;

function TScript.Execute: Variant;
begin
  for var i := 0 to Self.Count - 1 do
  begin
    var xSV := TSetVar(Self.Objects[i]);
    xSV.Execute;
  end;
  if FVars.IndexOfName('result') >= 0 then
    Result := StrToFloat(FVars.Values['result']);
end;

end.
