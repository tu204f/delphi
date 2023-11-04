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

  ///<summary>������� ������</summary>
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

  ///<summary>���������</summary>
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
    ///<summary>�������� ������� � ���������� ������������� ���������� ������� ��� ��������.</summary>
    property Portfolio: String read FPortfolio write FPortfolio;
    ///<summary>��������� �������� �������</summary>
    property Description: String read FDescription write FDescription;
    ///<summary>������ ����� �������� ����� �������</summary>
    ///<remarks>�������� ���������� ������ �����, � ������� ������ �������� ��������������</remarks>
    property ClientsList: String read GetClientsList write FClientsList;
    ///<summary>������ ��������������� ���� (���������� ������) ����� �������</summary>
    ///<remarks>�������� ���������� ������ �����, � ������� ������ �������� ��������������</remarks>
    property FirmsList: String read GetFirmsList write FFirmsList;
  end;

(******************************************************************************)
(******************************************************************************)

  ///<summary>���� ���������</summary>
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

  ///<summary>����������� �������� �������</summary>
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

      ///<summary>������ ������ � ������� �������, ����������� � �������. �������� ��� ���� ������</summary>
      ///<remarks>NUMERIC(<������_�����_�_��������>, <���_��_������_�����_�������>) - ������� � ��������� ������ (double)</remarks>
      function GetTypeParameterNumeric(ASizeNumberCharacters, ANumberCharacters: Integer): String;

      ///<summary>������ ������ � ������� �������, ����������� � �������. �������� ��� ���� ������</summary>
      ///<remarks>STRING(<�����_������>) - ��������� (string)</remarks>
      function GetTypeParameterString(ASizeLength: Integer): String;

      procedure Execute; override;
    public
      constructor Create(const AScript: TGeneratorScript); override;
      destructor Destroy; override;
      ///<summary>�������� ���������� � ��������� (������������ ����� 31 ������), �������� ������� ����� ������������ � ������ �������</summary>
      property Name: String read FName write SetName;
      ///<summary>�������� ������� (������������ ����� 31 ������), ������������ � �������</summary>
      property Title: String read FTitle write SetTitle;
      ///<summary>����������� �������� ��������� (������������ ����� 127 ��������)</summary>
      property Description: String read FDescription write SetDescription;
      ///<summary>������ ������ � ������� �������, ����������� � �������. �������� ��� ���� ������: NUMERIC(n,m), STRING(l)</summary>
      property TypeParameter: TTypeParameter read FType write FType;

      // ������ ������ � ������� �������, ����������� � �������. �������� ��� ���� ������:
      // - NUMERIC(<������_�����_�_��������>, <���_��_������_�����_�������>) - ������� � ��������� ������ (double);
      // - STRING(<�����_������>) - ��������� (string)
      property SizeNumberCharacters: Integer read FSizeNumberCharacters write FSizeNumberCharacters default 10;
      property NumberCharacters: Integer read FNumberCharacters write FNumberCharacters default 2;
      property SizeLengthCharacters: Integer read FSizeLengthCharacters write FSizeLengthCharacters default 256;
    end;

    ///<summary>������ ����������</summary>
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

  ///<summary>������������ �������</summary>
  TGeneratorScript = class(TObject)
  private
    FSource: TStrings;
    FHeader: THeader;
    FBody: TBody;
    FTableColumns: TTableColumns;
  protected
    ///<summary>�������� - ������ ������������</summary>
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
    raise Exception.Create('Error Message: �������� ���������� � ���������');
  if FTitle.IsEmpty then
    raise Exception.Create('Error Message: �������� �������');
  if FDescription.IsEmpty then
    raise Exception.Create('Error Message: ����������� �������� ���������');

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
  // ������� ��������  (������� ��� ������) � �������� �� ���������
  // ������������ ���������������. ��� ��������� ��������� �����������
  // ������������� � ������� �������.
  FSource.Add('USE_CASE_SENSITIVE_CONSTANTS');
  SetAddBlankSpace;
  FBody.Execute;
  SetAddBlankSpace;
  FTableColumns.Execute;
  // ����� �������� ���������� � ����� ����� �����������
  // ������ ���� ������ �END_PORTFOLIO� ��� ������ ������ �����,
  // ���� �END_PORTFOLIO_EX� ��� ��� ����� ������
  FSource.Add('END_PORTFOLIO_EX');
end;

end.
