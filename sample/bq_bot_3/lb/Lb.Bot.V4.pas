unit Lb.Bot.V4;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.SysUtils,
  Lb.Crossing;

type
  TWorkBotDeviation = class(TCrossing)
  private
    FCandel: TCandel;
    FCountValue: Integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Candel: TCandel read FCandel;
    property CountValue: Integer read FCountValue;
  end;

implementation

{ TWorkBotDeviation }

constructor TWorkBotDeviation.Create;
begin
  inherited;

end;

destructor TWorkBotDeviation.Destroy;
begin

  inherited;
end;

end.
