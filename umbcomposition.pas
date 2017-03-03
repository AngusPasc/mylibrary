unit UMBComposition;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, db;

type
    TMBComposition = Class
      private
             FCompositionName : String;
             FCompositionAnnotation : String;
             FCompositionAuthorsID : array of Integer;
             FNewComposition : Boolean;
             FCompositionID : Integer;
             function GetCompositionAuthorID(Index : Integer) : Integer;
             procedure GetCompositionAuthors(const SQLQuery :  TSQLQuery);
             procedure SetCompositionName( Name : String );
             procedure SetCompositionAnnotation( Annotation : String );
      published

      public
             constructor Create(); overload;
             constructor Create(Name : String; Annotation : String; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
             constructor Create(ID : Integer; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
             property CompositionName : String read FCompositionName write SetCompositionName;
             property CompositionAnnotation : String read FCompositionAnnotation write SetCompositionAnnotation;
             property CompositionID : Integer read FCompositionID;
             property CompositionsAuthor[Index : Integer]: Integer read GetCompositionAuthorID;
             procedure UpdateComposition(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);

    end;

implementation

constructor TMBComposition.Create();
begin
     FCompositionName:='';
     FCompositionAnnotation:='';
     FCompositionID:=0;
     FNewComposition := True;
end;

constructor TMBComposition.Create(Name : String; Annotation : String; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
begin
     SQLQuery.Close;
     SQLQuery.SQL.Text:='SELECT id, annotation FROM compositions where name=:bName'; //ДК, может LIMIT 1 воткнуть?
     SQLQuery.Params.ParamByName('bName').AsString:=Name;
     SQLQuery.Open;

     if SQLQuery.RecordCount > 0 then
     begin
          FCompositionName:=Name;
          FCompositionAnnotation:=Annotation;
          FCompositionID:=SQLQuery.FieldByName('id').AsInteger;
          FCompositionAnnotation:=SQLQuery.FieldByName('annotation').AsString;
          FNewComposition := False;
     end
     else
     begin
          //ДК, надо добавить проверку поле Name не должно быть пустым
          FCompositionName:=Name;
          FCompositionAnnotation:=Annotation;
          FNewComposition := True;
          UpdateComposition(SQLQuery, SQLTransaction);
     end;
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
     SQLQuery.Params.ParamByName('bID').AsString:=IntToStr(FCompositionID);
     SQLQuery.Open;
     SQLQuery.Last;
     if SQLQuery.RecordCount > 0 then
     begin
          SetLength(FCompositionAuthorsID, SQLQuery.RecordCount);
          SQLQuery.First;
          I:=0;
          while not SQLQuery.EOF do
          begin
               FCompositionAuthorsID[I]:=SQLQuery.FieldByName('author_id').AsInteger;
               I:=I+1;
               SQLQuery.Next;
          end;
          SQLQuery.Close;
     end;
end;

procedure TMBComposition.SetCompositionName(Name : String);
begin
     //ДК, тут должна быть проверка на допустимое значение CompositionName
     FCompositionName := Name;
end;

procedure TMBComposition.SetCompositionAnnotation(Annotation : String);
begin
     //ДК, тут должна быть проверка на допустимое значение CompositionAnnotation
     FCompositionAnnotation := Annotation;
end;

function TMBComposition.GetCompositionAuthorID(Index : Integer) : Integer;
begin
     if Index < Length(FCompositionAuthorsID) then
        Result := FCompositionAuthorsID[Index];
end;

procedure TMBComposition.UpdateComposition(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
begin
    if FNewComposition = True then
    begin
         SQLQuery.Close;
         SQLQuery.SQL.Text:='insert into compositions (name, annotation) values (:bCompositionName, :bCompositionAnnotation )';
         SQLQuery.Params.ParamByName('bCompositionName').AsString:=FCompositionName;
         SQLQuery.Params.ParamByName('bCompositionAnnotation').AsString:=FCompositionAnnotation;
         SQLQuery.ExecSQL;
         SQLTransaction.Commit;
         FNewComposition := False;
    end
    else
    begin
        SQLQuery.Close;
        SQLQuery.SQL.Text:='update compositions set name=:bCompositionName, annotation=:bCompositionAnnotation where id =:bCompositionID';
        SQLQuery.Params.ParamByName('bCompositionName').AsString:=FCompositionName;
        SQLQuery.Params.ParamByName('bCompositionAnnotation').AsString:=FCompositionAnnotation;
        SQLQuery.Params.ParamByName('bComposiionID').AsString:=IntToStr(FCompositionID);
        SQLQuery.ExecSQL;
        SQLTransaction.Commit;
    end;
         SQLQuery.Close;
end;



end.

