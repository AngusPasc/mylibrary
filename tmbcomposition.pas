unit UMBComposition;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, db;

type
    TMBComposition = Class
      private
             FCompositionName : String;
             FCompositionAnnatation : String
             FCompositionAurhorsID : array of Integer;
             FNewComposition : Boolean;
             function GetCompositionAuthorID(Index : Integer) : Integer;
             procedure GetCompositionAuthors(const SQLQuery :  TSQLQuery);
      published

      public
             constructor Create(Name : String = ""; Annotation : String = "");
             constructor Create(ID : Integer; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
             property CompositionName : String read FCompositionName write SetCompositionName;
             property CompositionAnnatation : String read FCompositionAnnatation write SetCompositionAnnotation;
             property CompositionID : String read FCompositionID;
             property CompositionsAuthor[Index : Integer]: FCompositionsAuthorsID read GetCompositionAuthorID;
             procedure UpdateComposition(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);

    end;

implementation

constructor TMBComposition.Create(Name : String = ""; Annotation : String = "");
begin
     //ДК, надо добавить проверку поле Name не должно быть пустым
     FCompositionName:=Name;
     FCompositionAnnotation:=Annotation;
     FCompositionID:=0;
     FNewComposition := True;
end;

constructor TMBComposition.Create(ID : Integer; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
begin
     SQLQuery.Close;
     SQLQuery.SQL.Text:='SELECT name, annotation FROM compositions where id=:bID';
     SQLQuery.Params.ParamByName('bID').AsString:=IntToStr(ID);
     SQLQuery.Open;
     //ДК, нужна проверка что запрос вернул не пустой результат
     FCompositionName:=SQLQuery.FieldByName('name').AsString;
     FCompositionAnnotation:=SQLQuery.FieldByName('annotation').AsString;
     FCompositionID:=ID;
     SQLQuery.Close;
     FNewComposition := False;
end;

procedure TMBComposition.GetCompositionAuthors(const SQLQuery :  TSQLQuery);
var
  I : Integer;
begin
     SQLQuery.Close;
     SQLQuery.SQL.Text:='SELECT author_id FROM rel_composition_authors where composition_id=:bID';
     SQLQuery.Params.ParamByName('bID').AsString:=IntToStr(ID);
     SQLQuery.Open;
     SQLQuery.Last;
     if SQLQuery.RecordCount > 0 then
     begin
          SetLength(FCompositionAuthorID, SQLQuery.RecordCount);
          SQLQuery.First;
          I:=0;
          while not SQLQuery.EOF do
          begin
               FCompositionsAuthorID[I]:=SQLQuery.FieldByName('author_id');
               I:=I+1;
               SQLQuery.Next;
          end;
          SQLQuery.Close;
     end;
end;

procedure TMBComposition.SetCompositionName(CompositionName : String);
begin
     //ДК, тут должна быть проверка на допустимое значение CompositionName
     FCompositionName := CompositionName;
end;

procedure TMBComposition.SetCompositionAnnotation(CompositionAnnotation : String);
begin
     //ДК, тут должна быть проверка на допустимое значение CompositionAnnotation
     FCompositionAnnotation := CompositionAnnotation;
end;

function TMBComposition.GetCompositionAuthorID(Index : Integer) : Integer;
begin
     if Index < Lenght(FCompositionAurhorsID) then
        Result := FcompositionAuthorID[Index];
end;

procedure TMBComposition.UpdateComposition(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
begin
    if FNewComposition = True then
    begin
         SQLQuery.Close;
         SQLQuery.SQL.Text:='inset into compositions (name, annotation) values (:bCompositionName, :bCompositionAnnotation )';
         SQLQuery.Params.ParamByName('bCompositionName').AsString:=FCompositionName;
         SQLQuery.Params.ParamByName('bCompositionAnnotation').AsString:=FCompositionAnnotation;
         SQLQuery.ExecSQL;
         SQLTransaction.Commit;
         FNewComposition := False;
    else
        SQLQuery.Close;
        SQLQuery.SQL.Text:='update composition set name=:bCompositionName annotation=:bCompositionAnnotation where id =:bCompositionID';
        SQLQuery.Params.ParamByName('bCompositionName').AsString:=FCompositionName;
        SQLQuery.Params.ParamByName('bCompositionAnnotation').AsString:=FCompositionAnnotation;
        SQLQuery.Params.ParamByName('bComposiionID').AsString:=FCompositionID;
        SQLQuery.ExecSQL;
        SQLTransaction.Commit;
    end;
         SQLQuery.Close;
end;

end.

