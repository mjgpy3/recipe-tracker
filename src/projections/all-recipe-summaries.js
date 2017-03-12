fromCategory('Recipe')
  .when({
    $init: function (_, __) {
      return [];
    },
    RecipeAdded: function (s, e) {
      var overallTime = e.body.recipe.overallTime;

      s.push({
        name: e.body.recipe.name,
        overallTime: overallTime ? overallTime + 'min' : 'Unknown time'
      });
    }
  });
