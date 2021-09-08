unit Lb.DomainFrame;

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
  TDomainFrame = class(TFrame)
  private
    FDomain: TCrDomain;
    procedure SetDomain(const Value: TCrDomain);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Domain: TCrDomain read FDomain write SetDomain;
  end;

implementation

{$R *.fmx}

{ TDomainFrame }

constructor TDomainFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

destructor TDomainFrame.Destroy;
begin

  inherited Destroy;
end;

procedure TDomainFrame.SetDomain(const Value: TCrDomain);
begin
  FDomain := Value;
end;

end.
