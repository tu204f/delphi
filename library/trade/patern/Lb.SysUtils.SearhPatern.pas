unit Lb.SysUtils.SearhPatern;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.SysUtils.Candel;

type
  TApplicationProcessMessages = procedure() of object;


  ///<summary>��������� �������</summary>
  TInfoPatern = record
    ///<summary>����� ������</summary>
    LineID: Integer;
    ///<summary>���������� ������� �� ����</summary>
    DifferencePrice: Double;
    ///<summary>���������� ������� �� ������</summary>
    DifferenceVol: Double;
  private
    function GetValue: String;
    procedure SetValue(const Value: String);
  public
    constructor Create(ALineID: Integer; ADifferencePrice, ADifferenceVol: Double);
    property Value: String read GetValue write SetValue;
  end;

  TEventInfoPatern = procedure(ASender: TObject; const AStructure, APatern: TVectorStructure; const AInfoPatern: TInfoPatern) of object;

  ///<summary>������ ��������</summary>
  TInfoPaterns = class(TStringList)
  private
    function GetPaterns(Index: Integer): TInfoPatern;
    procedure SetPaterns(Index: Integer; const Value: TInfoPatern);
  public
    ///<summary>�������� ���������� �������</summary>
    function AddInfoPatern(AInfoPatern: TInfoPatern): Integer; overload;
    function AddInfoPatern(ALineID: Integer; ADifferencePrice, ADifferenceVol: Double): Integer; overload;

    ///<summary>�������� ���������� �������</summary>
    procedure InsertInfoPatern(AInfoPatern: TInfoPatern); overload;
    procedure InsertInfoPatern(ALineID: Integer; ADifferencePrice, ADifferenceVol: Double); overload;

    property Paterns[Index: Integer]: TInfoPatern read GetPaterns write SetPaterns;
  end;


  ///<summary>����� �������</summary>
  TSearchPatern = class(TObject)
  private
    FMemoryStructures: TMemoryStructures;
    FVectorStructure: TVectorStructure;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    ///<summary>�������� ������ ��������</summary>
    procedure GenerationInfoPaterns(const AVectorStructure: TVectorStructure; const ApplicationProcessMessages: TApplicationProcessMessages = nil);
  end;

implementation



{ TInfoPatern }

constructor TInfoPatern.Create(ALineID: Integer; ADifferencePrice, ADifferenceVol: Double);
begin
  LineID := ALineID;
  DifferencePrice := ADifferencePrice;
  DifferenceVol := ADifferenceVol;
end;

function TInfoPatern.GetValue: String;
begin
  Result :=
    LineID.ToString + ';' +
    DifferencePrice.ToString + ';' +
    DifferenceVol.ToString + ';';
end;

procedure TInfoPatern.SetValue(const Value: String);
var
  xStr: TStrings;
begin
  xStr := TStringList.Create;
  try
    xStr.Delimiter := ';';
    xStr.DelimitedText := Value;
    if xStr.Count > 3 then
    begin
      LineID := xStr[0].ToInteger;
      DifferencePrice := xStr[1].ToDouble;
      DifferenceVol := xStr[2].ToDouble;
    end
    else
      raise Exception.Create('Error Message: TInfoPatern.SetValue - �� ��������� ��������� ������');
  finally
    FreeAndNil(xStr);
  end;
end;

{ TInfoPaterns }

function TInfoPaterns.AddInfoPatern(AInfoPatern: TInfoPatern): Integer;
begin
  Result := Self.Add(AInfoPatern.Value);
end;

function TInfoPaterns.AddInfoPatern(ALineID: Integer; ADifferencePrice, ADifferenceVol: Double): Integer;
begin
  Result := Self.AddInfoPatern(TInfoPatern.Create(ALineID, ADifferencePrice, ADifferenceVol));
end;

function TInfoPaterns.GetPaterns(Index: Integer): TInfoPatern;
var
  xS: String;
begin
  if (Index < 0) or (Index >= Self.Count) then
    raise Exception.Create('Error Message: TInfoPaterns.GetPaterns ����� �� ������� �������');
  xS := Strings[Index];
  Result.Value := xS;
end;

procedure TInfoPaterns.SetPaterns(Index: Integer; const Value: TInfoPatern);
var
  xS: String;
begin
  if (Index < 0) or (Index >= Self.Count) then
    raise Exception.Create('Error Message: TInfoPaterns.SetPaterns ����� �� ������� �������');
  xS := Value.Value;
  Strings[Index] := xS;
end;

procedure TInfoPaterns.InsertInfoPatern(ALineID: Integer; ADifferencePrice, ADifferenceVol: Double);
begin
  Self.InsertInfoPatern(TInfoPatern.Create(ALineID, ADifferencePrice, ADifferenceVol));
end;

procedure TInfoPaterns.InsertInfoPatern(AInfoPatern: TInfoPatern);

  function _SearchInfoPatern(AInfoPatern: TInfoPatern): Integer;
  var
    i, iCount: Integer;
    xInfoPatern: TInfoPatern;
  begin
    Result := 0;
    iCount := Self.Count;
    if iCount > 0  then
      for i := 0 to iCount - 1 do
      begin
        xInfoPatern := Self.Paterns[i];
        if xInfoPatern.DifferencePrice > AInfoPatern.DifferencePrice then
        begin
          Result := i;
          Break;
        end;
      end;
  end;

var
  xIndex: Integer;
begin
  xIndex := _SearchInfoPatern(AInfoPatern);
  Self.Insert(xIndex,AInfoPatern.Value);
end;

{ TSearchPatern }

constructor TSearchPatern.Create;
begin
  FMemoryStructures := TMemoryStructures.Create;
  FVectorStructure := TVectorStructure.Create;
end;

destructor TSearchPatern.Destroy;
begin
  FreeAndNil(FVectorStructure);
  FreeAndNil(FMemoryStructures);
  inherited;
end;

procedure TSearchPatern.GenerationInfoPaterns(const AVectorStructure: TVectorStructure; const ApplicationProcessMessages: TApplicationProcessMessages);
var
  xVectorStructure: TVectorStructure;
  xLengthPrice, xLengthVol: Double;
  xSourceRowID: Integer;
var
  xInfoPatern: TInfoPatern;
begin
  FVectorStructure.Assign(AVectorStructure);
  while not FMemoryStructures.EOF do
  begin
    xVectorStructure := TVectorStructure.Create;
    try
      xVectorStructure.Transform(FMemoryStructures.Structure);
      TMathVector.SetSubtractStructure(xVectorStructure,FVectorStructure,xLengthPrice, xLengthVol);
      xSourceRowID := FMemoryStructures.Structure.SourceRowID;

      with xInfoPatern do
      begin
        LineID := xSourceRowID;
        DifferencePrice := xLengthPrice;
        DifferenceVol := xLengthVol;
      end;

      FMemoryStructures.NextStructure;
    finally
      FreeAndNil(xVectorStructure);
    end;
  end;

end;

end.
