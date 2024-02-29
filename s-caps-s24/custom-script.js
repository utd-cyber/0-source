document.addEventListener("DOMContentLoaded", function() {
    document.querySelectorAll('.details-list').forEach(function(item) {
        var summary = item.querySelector('.details-summary');
        summary.addEventListener('click', function() {
            var content = item.querySelector('.details-content');
            content.style.display = content.style.display === 'none' ? 'block' : 'none';
        });
    });
});
