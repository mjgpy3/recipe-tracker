fromCategory('Recipe')
  .when({
    $init: function (_, __) {
      return [];
    },
    RecipeAdded: function (s, e) {
      s.push(e.body.recipe.name);
    }
  });
