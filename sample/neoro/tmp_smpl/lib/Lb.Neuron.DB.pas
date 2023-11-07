unit Lb.Neuron.DB;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections;

const
  SOURCE_COUNT = 60000;
  SOURCE_TEST  = 10000;

type
  TBufferImage = array[0..27,0..27] of Byte;

  TLableList = TList<Byte>;
  TBufferImageList = TList<TBufferImage>;

var
  SourceLabel: TLableList = nil;
  SourceImage: TBufferImageList = nil;
  TestSourceLabel: TLableList = nil;
  TestSourceImage: TBufferImageList = nil;

const
  Names: array [0 .. 9] of String = ('T-shirt/top', 'Trouser', 'Pullover',
    'Dress', 'Coat', 'Sandal', 'Shirt', 'Sneaker', 'Bag', 'Ankle boot');

procedure LoadSourceImage(AFileName: String; AImages: TBufferImageList);
procedure LoadSourceLabel(AFileName: String; ALabels: TLableList);

implementation

// t10k-images.idx3-ubyte
// t10k-labels.idx1-ubyte
// train-images.idx3-ubyte
// train-labels.idx1-ubyte

const
  PATH_SOURCE = 'd:\work\git\delphi\sample\neoro\data\';

procedure LoadSourceImage(AFileName: String; AImages: TBufferImageList);
var
  xFN: String;
  xStream: TFileStream;
  xMagicNumber: Cardinal;
  xCount: Cardinal;
begin
  xFN := PATH_SOURCE + AFileName;
  xStream := TFileStream.Create(xFN,fmOpenReadWrite);
  try

  finally
    FreeAndNil(xStream);
  end;
end;

function Get—ross(const AValue: Integer): Integer;
begin

end;

procedure LoadSourceLabel(AFileName: String; ALabels: TLableList);
var
  xFN: String;
  xStream: TFileStream;
  xMagicNumber: Integer;
  xCount: Integer;
begin
  xFN := PATH_SOURCE + AFileName;
  xStream := TFileStream.Create(xFN,fmOpenReadWrite);
  try
    xStream.Position := 0;
    xStream.Read(xMagicNumber,4);
    xMagicNumber := Get—ross(xMagicNumber);

    xStream.Read(xCount,4);
    xCount := Get—ross(xCount);

  finally
    FreeAndNil(xStream);
  end;
end;


initialization
  SourceLabel     := TLableList.Create;
  SourceImage     := TBufferImageList.Create;
  TestSourceLabel := TLableList.Create;
  TestSourceImage := TBufferImageList.Create;

finalization
  FreeAndNil(SourceLabel);
  FreeAndNil(SourceImage);
  FreeAndNil(TestSourceLabel);
  FreeAndNil(TestSourceImage);

end.
