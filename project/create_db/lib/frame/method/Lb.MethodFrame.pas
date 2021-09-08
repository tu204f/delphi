unit Lb.MethodFrame;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  Lb.Create.DB;

type
  TMethodFrame = class(TFrame)
  private
    FMethod: TCrMethod;
    procedure SetMethod(const Value: TCrMethod);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Method: TCrMethod read FMethod write SetMethod;
  end;

implementation

{$R *.fmx}

{ TMethodFrame }

constructor TMethodFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMethod := nil;
end;

destructor TMethodFrame.Destroy;
begin

  inherited;
end;

procedure TMethodFrame.SetMethod(const Value: TCrMethod);
begin
  FMethod := Value;
end;

end.
