unit Lb.Source.DB;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Contnrs,
  System.Generics.Collections,
  Vcl.Graphics;

type
  TByteList = TList<Byte>;
  TArrayByte = TArray<Byte>;
  TMaskImg = array [1 .. 28, 1 .. 28] of Single;

  TCustomSourceDataSet = class(TObject)
  private
    FMagic: Integer;
    FCount: Integer;
    FSources: TStream;
  protected
    procedure SetSources(const Value: TStream); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Sources: TStream read FSources write SetSources;
    property Magic: Integer read FMagic;
    property Count: Integer read FCount;
  end;

  TSourceDataSetLabels = class(TCustomSourceDataSet)
  private
    FBytes: TByteList;
  protected
    procedure SetSources(const Value: TStream); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Bytes: TByteList read FBytes;
  end;

  TSourceImage = class(TObject)
  private
    FBytes: TByteList;
    FCountRows: Integer;
    FCountColums: Integer;
  public
    constructor Create(const ArrayBytes: TArrayByte; const ACountRows, ACountColums: Integer); virtual;
    destructor Destroy; override;
    property Bytes: TByteList read FBytes;
    property CountRows: Integer read FCountRows;
    property CountColums: Integer read FCountColums;
  end;
  TSourceImageList = TObjectList<TSourceImage>;

  TSourceDataSetImages = class(TCustomSourceDataSet)
  private
    FSourceImages: TSourceImageList;
  protected
    procedure SetSources(const Value: TStream); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property SourceImages: TSourceImageList read FSourceImages;
  end;

var
  Names: array [0 .. 9] of String = (
    'T-shirt/top',
    'Trouser',
    'Pullover',
    'Dress',
    'Coat',
    'Sandal',
    'Shirt',
    'Sneaker',
    'Bag',
    'Ankle boot'
  );

procedure SourceImageToBitmap(const ASourceImage: TSourceImage; const ABitmap: TBitmap);
function SourceImageToMaskImg(const ASourceImage: TSourceImage): TMaskImg;

implementation

function GetCrossInteger(const AValue: Integer): Integer;
var
  xValues: array [0..3] of Byte;
  xTmps: array [0..3] of Byte;
  xValueTmp: Integer;
  i: Integer;
begin
  Move(AValue,xValues,4);
  for i := 0 to 3 do
    xTmps[i] := xValues[3 - i];
  Move(xTmps,xValueTmp,4);
  Result := xValueTmp;
end;

procedure SourceImageToBitmap(const ASourceImage: TSourceImage; const ABitmap: TBitmap);
var
  i, j, xIndex: Integer;
  xB: Byte;
begin
  ABitmap.Height := ASourceImage.CountRows;
  ABitmap.Width  := ASourceImage.CountColums;

  for j := 0 to ASourceImage.CountRows - 1 do
    for i := 0 to ASourceImage.CountColums - 1 do
    begin
      xIndex := i * ASourceImage.CountRows + j;
      xB := ASourceImage.Bytes[xIndex];
      ABitmap.Canvas.Pixels[j,i] := xB;
    end;
end;

function SourceImageToMaskImg(const ASourceImage: TSourceImage): TMaskImg;
var
  i, j, xIndex: Integer;
  xB: Byte;
  xMaskImg: TMaskImg;
begin
  for j := 0 to ASourceImage.CountRows - 1 do
    for i := 0 to ASourceImage.CountColums - 1 do
    begin
      xIndex := i * ASourceImage.CountRows + j;
      xB := ASourceImage.Bytes[xIndex];
      xMaskImg[j + 1,i + 1] := xB/255;
    end;
  Result := xMaskImg;
end;

{ TCustomSourceDataSet }

constructor TCustomSourceDataSet.Create;
begin
  FMagic := 0;
  FCount := 0;
end;

destructor TCustomSourceDataSet.Destroy;
begin

  inherited;
end;

procedure TCustomSourceDataSet.SetSources(const Value: TStream);
begin
  FSources := Value;
  if not Assigned(FSources) then
    raise Exception.Create('Неопредел TStream данных');
  FSources.Position := 0;
  FSources.Read(FMagic,4);
  FMagic := GetCrossInteger(FMagic);
  FSources.Read(FCount,4);
  FCount := GetCrossInteger(FCount);
end;

{ TSourceDataSetLabels }

constructor TSourceDataSetLabels.Create;
begin
  inherited;
  FBytes := TByteList.Create;
end;

destructor TSourceDataSetLabels.Destroy;
begin
  FreeAndNil(FBytes);
  inherited;
end;

procedure TSourceDataSetLabels.SetSources(const Value: TStream);
var
  xByte: Byte;
  i: Integer;
begin
  inherited;
  FBytes.Clear;
  for i := 0 to FCount - 1 do
  begin
    FSources.Read(xByte,1);
    FBytes.Add(xByte);
  end;
end;

{ TSourceImage }

constructor TSourceImage.Create(const ArrayBytes: TArrayByte; const ACountRows, ACountColums: Integer);
begin
  FCountRows := ACountRows;
  FCountColums := ACountColums;
  FBytes := TByteList.Create;
  for var xB in ArrayBytes do
    FBytes.Add(xB);
end;

destructor TSourceImage.Destroy;
begin
  FreeAndNil(FBytes);
  inherited;
end;

{ TSourceImages }

constructor TSourceDataSetImages.Create;
begin
  inherited;
  FSourceImages := TSourceImageList.Create;
end;

destructor TSourceDataSetImages.Destroy;
begin
  FreeAndNil(FSourceImages);
  inherited;
end;

procedure TSourceDataSetImages.SetSources(const Value: TStream);
var
  xByte: Byte;
  xCountRows: Integer;
  xCountColums: Integer;
  xCountBytes: Integer;
  xImage: TSourceImage;
  xBytes: TArrayByte;
begin
  inherited;
  FSources.ReadBuffer(xCountRows,4);
  xCountRows := GetCrossInteger(xCountRows);
  FSources.ReadBuffer(xCountColums,4);
  xCountColums := GetCrossInteger(xCountColums);

  xCountBytes := xCountRows * xCountColums;
  SetLength(xBytes,xCountBytes);

  FSourceImages.Clear;
  for var i := 0 to FCount - 1 do
  begin
    FSources.Read(xBytes,xCountBytes);
    xImage := TSourceImage.Create(xBytes,xCountRows,xCountColums);
    FSourceImages.Add(xImage);
  end;
end;

end.
