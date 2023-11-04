unit QPILE.Script;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Generics.Collections;

type
  TGeneratorScript = class;

  ///<summary>Базовый объект</summary>
  TCustomModule = class(TObject)
  private
    FScript: TGeneratorScript;
  protected
    procedure Execute; virtual;
    property Script: TGeneratorScript read FScript;
  public
    constructor Create(const AScript: TGeneratorScript); virtual;
    destructor Destroy; override;
  end;

(******************************************************************************)
(******************************************************************************)

  ///<summary>Заголовок</summary>
  THeader = class(TCustomModule)
  private
    FPortfolio: String;
    FDescription: String;
    FClientsList: String;
    FFirmsList: String;
    function GetClientsList: String;
    function GetFirmsList: String;
  protected
    procedure Execute; override;
  public
    constructor Create(const AScript: TGeneratorScript); override;
    destructor Destroy; override;
    ///<summary>Название таблицы – символьный идентификатор латинскими буквами без пробелов.</summary>
    property Portfolio: String read FPortfolio write FPortfolio;
    ///<summary>Текстовое описание таблицы</summary>
    property Description: String read FDescription write FDescription;
    ///<summary>Список кодов клиентов через запятую</summary>
    ///<remarks>параметр предыдущей версии языка, в текущей версии является необязательным</remarks>
    property ClientsList: String read GetClientsList write FClientsList;
    ///<summary>Список идентификаторов фирм (участников торгов) через запятую</summary>
    ///<remarks>параметр предыдущей версии языка, в текущей версии является необязательным</remarks>
    property FirmsList: String read GetFirmsList write FFirmsList;
  end;

(******************************************************************************)
(******************************************************************************)

  ///<summary>Тело программы</summary>
  TBody = class(TCustomModule)
  private
  protected
    procedure Execute; override;
  public
    constructor Create(const AScript: TGeneratorScript); override;
    destructor Destroy; override;
  end;

(******************************************************************************)
(******************************************************************************)

  ///<summary>Определение столбцов таблицы</summary>
  TTableColumns = class(TCustomModule)
  public type
    TTypeParameter = (tpNumeric,tpString);

    TParameter = class(TCustomModule)
    private
      FName: String;
      FTitle: String;
      FDescription: String;
      FType: TTypeParameter;

      FSizeNumberCharacters: Integer;
      FNumberCharacters: Integer;
      FSizeLengthCharacters: Integer;

      procedure SetName(const Value: String);
      procedure SetTitle(const Value: String);
      procedure SetDescription(const Value: String);
    protected

      ///<summary>Формат данных в ячейках таблицы, относящихся к столбцу. Возможны два типа данных</summary>
      ///<remarks>NUMERIC(<размер_числа_в_символах>, <кол_во_знаков_после_запятой>) - двойные с плавающей точкой (double)</remarks>
      function GetTypeParameterNumeric(ASizeNumberCharacters, ANumberCharacters: Integer): String;

      ///<summary>Формат данных в ячейках таблицы, относящихся к столбцу. Возможны два типа данных</summary>
      ///<remarks>STRING(<длина_строки>) - строковые (string)</remarks>
      function GetTypeParameterString(ASizeLength: Integer): String;

      procedure Execute; override;
    public
      constructor Create(const AScript: TGeneratorScript); override;
      destructor Destroy; override;
      ///<summary>Название переменной в программе (максимальная длина 31 символ), значение которой будет отображаться в данном столбце</summary>
      property Name: String read FName write SetName;
      ///<summary>Название столбца (максимальная длина 31 символ), отображаемое в таблице</summary>
      property Title: String read FTitle write SetTitle;
      ///<summary>Расширенное описание параметра (максимальная длина 127 символов)</summary>
      property Description: String read FDescription write SetDescription;
      ///<summary>Формат данных в ячейках таблицы, относящихся к столбцу. Возможны два типа данных: NUMERIC(n,m), STRING(l)</summary>
      property TypeParameter: TTypeParameter read FType write FType;

      // Формат данных в ячейках таблицы, относящихся к столбцу. Возможны два типа данных:
      // - NUMERIC(<размер_числа_в_символах>, <кол_во_знаков_после_запятой>) - двойные с плавающей точкой (double);
      // - STRING(<длина_строки>) - строковые (string)
      property SizeNumberCharacters: Integer read FSizeNumberCharacters write FSizeNumberCharacters default 10;
      property NumberCharacters: Integer read FNumberCharacters write FNumberCharacters default 2;
      property SizeLengthCharacters: Integer read FSizeLengthCharacters write FSizeLengthCharacters default 256;
    end;

    ///<summary>Список параметров</summary>
    TParameterList = TObjectList<TParameter>;

  private
    FParameters: TParameterList;
  protected
    procedure Execute; override;
  public
    constructor Create(const AScript: TGeneratorScript); override;
    destructor Destroy; override;
    procedure Clear;
    function AddParameter: TParameter;
    property Parameters: TParameterList read FParameters;
  end;

  ///<summary>Генерировать скрипта</summary>
  TGeneratorScript = class(TObject)
  private
    FSource: TStrings;
    FHeader: THeader;
    FBody: TBody;
    FTableColumns: TTableColumns;
  protected
    ///<summary>Добавить - Пустое пространство</summary>
    procedure SetAddBlankSpace;
    property Source: TStrings read FSource;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute;
    property Header: THeader read FHeader;
    property Body: TBody read FBody;
    property TableColumns: TTableColumns read FTableColumns;
  end;

implementation

function GetParams(const AName, AValue: String): String;
begin
  Result := Format('%s %s;',[AName,AValue]);
end;

{ TCustomModule }

constructor TCustomModule.Create(const AScript: TGeneratorScript);
begin
  FScript := AScript;
end;

destructor TCustomModule.Destroy;
begin

  inherited;
end;

procedure TCustomModule.Execute;
begin

end;

{ THeader }

constructor THeader.Create(const AScript: TGeneratorScript);
begin
  inherited Create(AScript);
end;

destructor THeader.Destroy;
begin

  inherited;
end;

function THeader.GetClientsList: String;
begin
  FClientsList := 'ALL_CLIENTS';
  Result := FClientsList;
end;

function THeader.GetFirmsList: String;
begin
  FFirmsList := 'ALL_FIRMS';
  Result := FFirmsList;
end;

procedure THeader.Execute;
begin
  inherited Execute;
  with Script.Source do
  begin
    Add(GetParams('PORTFOLIO_EX',FPortfolio));
    Add(GetParams('DESCRIPTION',FDescription));
    Add(GetParams('CLIENTS_LIST',FClientsList));
    Add(GetParams('FIRMS_LIST',FFirmsList));
  end;
end;

{ TBody }

constructor TBody.Create(const AScript: TGeneratorScript);
begin
  inherited Create(AScript);
end;

destructor TBody.Destroy;
begin

  inherited;
end;

procedure TBody.Execute;
begin
  inherited Execute;
  Script.Source.Add('PROGRAM');

  Script.Source.Add('END_PROGRAM');
end;

{ TTableColumns.TParameter }

constructor TTableColumns.TParameter.Create(const AScript: TGeneratorScript);
begin
  inherited Create(AScript);

end;

destructor TTableColumns.TParameter.Destroy;
begin

  inherited;
end;

procedure TTableColumns.TParameter.Execute;
begin
  inherited;

  if FName.IsEmpty then
    raise Exception.Create('Error Message: Название переменной в программе');
  if FTitle.IsEmpty then
    raise Exception.Create('Error Message: Название столбца');
  if FDescription.IsEmpty then
    raise Exception.Create('Error Message: Расширенное описание параметра');

  with Script.Source do
  begin
    Add(GetParams('PARAMETER',FName));
    Add(GetParams('PARAMETER_TITLE',FTitle));
    Add(GetParams('PARAMETER_DESCRIPTION',FDescription));
    case FType of
      TTypeParameter.tpNumeric: begin
        Add(GetParams(
          'PARAMETER_TYPE',
          GetTypeParameterNumeric(
            FSizeNumberCharacters,
            FNumberCharacters
          )
        ));
      end;
      TTypeParameter.tpString: begin
        Add(GetParams(
          'PARAMETER_TYPE',
          GetTypeParameterString(FSizeLengthCharacters)
        ));
      end;
    end;
    Add('END');
  end;
  Script.SetAddBlankSpace;
end;

function TTableColumns.TParameter.GetTypeParameterNumeric(ASizeNumberCharacters, ANumberCharacters: Integer): String;
begin
  Result := Format('NUMERIC (%d,%d)',[ASizeNumberCharacters, ANumberCharacters]);
end;

function TTableColumns.TParameter.GetTypeParameterString(ASizeLength: Integer): String;
begin
  Result := Format('STRING (%d)',[ASizeLength]);
end;

procedure TTableColumns.TParameter.SetName(const Value: String);
begin
  FName := Copy(Value,1,31);
end;

procedure TTableColumns.TParameter.SetTitle(const Value: String);
begin
  FTitle := Copy(Value,1,31);
end;

procedure TTableColumns.TParameter.SetDescription(const Value: String);
begin
  FDescription := Copy(Value,1,127);
end;

{ TTableColumns }

constructor TTableColumns.Create(const AScript: TGeneratorScript);
begin
  inherited Create(AScript);
  FParameters := TParameterList.Create;
end;

destructor TTableColumns.Destroy;
begin
  FreeAndNil(FParameters);
  inherited;
end;

procedure TTableColumns.Clear;
begin
  FParameters.Clear;
end;

function TTableColumns.AddParameter: TParameter;
var
  xParameter: TParameter;
begin
  xParameter := TParameter.Create(FScript);
  FParameters.Add(xParameter);
  Result := xParameter;
end;

procedure TTableColumns.Execute;
begin
  inherited Execute;
  for var xParameter in FParameters do
    xParameter.Execute;
end;

{ TGeneratorScript }

constructor TGeneratorScript.Create;
begin
  FSource := TStringList.Create;
  FHeader := THeader.Create(Self);
  FBody := TBody.Create(Self);
  FTableColumns := TTableColumns.Create(Self);
end;

destructor TGeneratorScript.Destroy;
begin
  FreeAndNil(FTableColumns);
  FreeAndNil(FBody);
  FreeAndNil(FHeader);
  FreeAndNil(FSource);
  inherited;
end;

procedure TGeneratorScript.SetAddBlankSpace;
begin
  for var i := 0 to 1 do
    FSource.Add('');
end;

procedure TGeneratorScript.Execute;
begin
  FSource.Clear;
  FHeader.Execute;
  SetAddBlankSpace;
  // Регистр символов  (верхний или нижний) в командах по умолчанию
  // игнорируется интерпретатором. Все строковые константы переводятся
  // автоматически в верхний регистр.
  FSource.Add('USE_CASE_SENSITIVE_CONSTANTS');
  SetAddBlankSpace;
  FBody.Execute;
  SetAddBlankSpace;
  FTableColumns.Execute;
  // После описания параметров в конце файла обязательно
  // должна быть строка «END_PORTFOLIO» для первой версии языка,
  // либо «END_PORTFOLIO_EX» для его новой версии
  FSource.Add('END_PORTFOLIO_EX');
end;

end.
