unit Lb.GeneticAlgorithm;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Contnrs,
  System.Generics.Collections;

type
  ///<summary>Хромосома<summary>
  TChromosomes = class(TObject)
  public type
    TGenList = TList<Double>;
  private
    FGens: TGenList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    {todo: формировать геном - из TNeuronNet - по весам}
    {todo: Запольнять веса TNeuronNet из генома}
  end;

  TGA = record
    class function GetBytesToValue(const AValue: Double): TBytes; static;
    class function GetValueToBytes(const AValue: TBytes): Double; static;
    class function GetMutation(const AValue: TBytes): TBytes; static;
    class function GetValuesCross(const AValue1, AValue2: Double): Double; static;
  end;

implementation

const
  SIZE_TYPE_DOUBLE = SizeOf(Double);

{ TChromosomes }

constructor TChromosomes.Create;
begin
  FGens := TGenList.Create;
end;

destructor TChromosomes.Destroy;
begin
  FreeAndNil(FGens);
  inherited;
end;

{ TGA }

class function TGA.GetBytesToValue(const AValue: Double): TBytes;
var
  xBytes: TBytes;
  xMemory: TMemoryStream;
begin
  xMemory := TMemoryStream.Create;
  try
    xMemory.Write(AValue,SIZE_TYPE_DOUBLE);
    xMemory.Position := 0;
    SetLength(xBytes,SIZE_TYPE_DOUBLE);
    xMemory.Read(xBytes,SIZE_TYPE_DOUBLE);
    Result := xBytes;
  finally
    FreeAndNil(xMemory);
  end;
end;

class function TGA.GetValueToBytes(const AValue: TBytes): Double;
var
  xValue: Double;
  xMemory: TMemoryStream;
begin
  xValue := 0;
  xMemory := TMemoryStream.Create;
  try
    xMemory.Write(AValue,SIZE_TYPE_DOUBLE);
    xMemory.Position := 0;
    xMemory.Read(xValue,SIZE_TYPE_DOUBLE);
    Result := xValue;
  finally
    FreeAndNil(xMemory);
  end;
end;

class function TGA.GetMutation(const AValue: TBytes): TBytes;
var
  xL: Integer;
  xByte: Byte;
  xBytes: TBytes;
begin
  xL := Length(AValue);
  SetLength(xBytes,xL);
  for var i := 0 to xL do
  begin
    xByte := AValue[i];
    xByte := Random(256);
    xBytes[i] := xByte;
  end;
  Result := xBytes;
end;

class function TGA.GetValuesCross(const AValue1, AValue2: Double): Double;
begin
  Result := 0;
end;




end.
